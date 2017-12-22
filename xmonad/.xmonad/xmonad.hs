{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent ( forkIO )
import Control.Concurrent.Chan
import Control.Monad ( forever )
import Data.List ( sortBy, intercalate, elemIndex, findIndex )
import Data.IORef ( writeIORef, readIORef, newIORef )
import qualified Data.Map as M
import Data.Ord ( comparing )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Word ( Word32 )
import Network.HTTP.Types.URI
import Network.URI
import System.Exit
import System.IO(hPutStrLn)

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow ( copy, kill1 )
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.NoBorders ( smartBorders )
import XMonad.Layout.Spacing
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Prompt.Shell
import qualified XMonad.StackSet as W
import XMonad.Util.Run

-- this is how the physical screen IDs end up layed out, from left to right.
-- 0 is the laptop monitor, and is typically in the middle.
-- 1 is the VGA port, usually on the left.
-- 2 is the displayport, usually on the right.
screenIds :: [Int]
screenIds = [ 0, 1, 2 ]

myScreenWidth = 1920

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal :: String
myTerminal = "urxvt"

-- Width of the window border in pixels.
myBorderWidth :: Word32
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask = mod4Mask

-- Default workspaces
myWorkspaces = [ "shell", "ff", "w", "d", "s" ]

-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor = solarizedBlue
myFocusedBorderColor = solarizedRed

-- | A rectangle: x, y, w, h.
type Rect = (Word32, Word32, Word32, Word32)

getLeftmostScreenRect :: X Rectangle
getLeftmostScreenRect = do
  ws <- gets windowset
  let screenRects = screenRect . W.screenDetail <$> W.current ws : W.visible ws
  pure $ head (sortBy (comparing rect_x) screenRects)

getCurrentScreenRect :: X Rectangle
getCurrentScreenRect = gets (screenRect . W.screenDetail . W.current . windowset)

-- | Gets a nice rectangle for spawning dmenu.
-- The algorithm splits the screen of the current workspace into a 3x3
-- grid, and returns the rectangle of the center middle cell.
dmenuDim :: X Rect
dmenuDim = do
  d <- getCurrentScreenRect
  let (w, h) = (rect_width d, rect_height d)
  let (dw, dh) = (w `div` 3, h `div` 3)
  let (dx, dy) = (w `div` 3, h `div` 3)
  pure (dx, dy, dw, dh)

data DmenuSettings =
  DmenuSettings
  { dsProgramName :: String
  -- ^ The name of the dmenu program to run, typically @"dmenu"@ or @"dmenu_run"@.
  , dsSpawnRect :: Rect
  -- ^ The location to spawn dmenu at.
  , dsPrompt :: String
  -- ^ The prompt to display.
  , dsInput :: Bool
  -- ^ Does dmenu read stdin for choices?
  , dsLines :: DmenuLineCount
  -- ^ How many lines of choices does dmenu display?
  }

data DmenuLineCount
  = FromHeight
  | Fixed Int
  | Unspecified

dmenuCommand :: DmenuSettings -> String
dmenuCommand DmenuSettings{..} = concat
  [ dsProgramName
  , " -dim 0.3 -fn 'mononoki'"
  , " -x ", show x
  , " -y ", show y
  , " -w ", show w
  , " " ++ ls
  , " -p '", dsPrompt, "'"
  ]
  where
    (x, y, w, h) = dsSpawnRect
    input = if dsInput then "" else "-noinput"
    ls = case dsLines of
      FromHeight -> "-l " ++ show (h `div` 20)
      Unspecified -> ""
      Fixed n -> "-l " ++ show n

spawnDmenu :: X ()
spawnDmenu = do
  dim <- dmenuDim
  spawnHere $ dmenuCommand DmenuSettings
    { dsProgramName = "dmenu_run"
    , dsSpawnRect = dim
    , dsPrompt = "$ "
    , dsLines = FromHeight
    , dsInput = True
    }

-- | Constructs a URI for performing a web search of the given string.
searchURI :: String -> ShowS
searchURI q = uriToString id $ URI
  { uriScheme = "https"
  , uriAuthority = Just URIAuth
    { uriUserInfo = ""
    , uriRegName = "duckduckgo.com"
    , uriPort = ""
    }
  , uriPath = ""
  , uriQuery = out $ renderQuery False [("q", Just $ t q)]
  }
  where
    out = T.unpack . T.decodeUtf8
    t = T.encodeUtf8 . T.pack

-- spawns dmenu for performing a web search with the given prompt
spawnDmenuWeb :: String -> X ()
spawnDmenuWeb p = do
  spawn =<< dmenuCommand . mkSettings <$> dmenuDim
  where
    mkSettings dim = DmenuSettings
      { dsProgramName = "dmenu"
      , dsSpawnRect = dim
      , dsPrompt = p
      , dsLines = Fixed 0
      , dsInput = False
      }

basicPrompt :: XPConfig
basicPrompt = def
  { font = "xft:mononoki:size=9"
  , fgColor = "#839496"
  , bgColor = "#002b36"
  , fgHLight = "#268bd2"
  , borderColor = "#586e75"
  , bgHLight = "#073642"
  , maxComplRows = Just 12
  , position = CenteredAt 0.5 0.5
  }

-- | Representation of a XPrompt instance as a record.
data SomePrompt = SomePrompt
  { _showXPrompt :: String
  , _nextCompletion :: String -> [String] -> String
  , _completionToCommand :: String -> String
  , _commandToComplete :: String -> String
  , _completionFunction :: ComplFunction
  , _modeAction :: String -> String -> X ()
  }

instance XPrompt SomePrompt where
  showXPrompt SomePrompt{..} = _showXPrompt
  nextCompletion SomePrompt{..} = _nextCompletion
  commandToComplete SomePrompt{..} = _commandToComplete
  completionToCommand SomePrompt{..} = _completionToCommand
  completionFunction SomePrompt{..} = _completionFunction
  modeAction SomePrompt{..} = _modeAction

instance Default SomePrompt where
  def = SomePrompt
    { _showXPrompt = ""
    , _nextCompletion = getNextOfLastWord (def :: SomePrompt)
    , _commandToComplete = getLastWord
    , _completionToCommand = id
    , _completionFunction = const (pure $ ["Completions could not be loaded."])
    , _modeAction = const (const (pure ()))
    }

myShellPrompt :: SomePrompt
myShellPrompt = def
  { _showXPrompt = "$ "
  , _completionToCommand = escape
  }

escape :: String -> String
escape []       = ""
escape (x:xs)
    | isSpecialChar x = '\\' : x : escape xs
    | otherwise       = x : escape xs

type Predicate = String -> String -> Bool

isSpecialChar :: Char -> Bool
isSpecialChar =  flip elem (" &\\@\"'#?$*()[]{};" :: String)

runMyShellPrompt :: XPConfig -> X ()
runMyShellPrompt c = do
  cmds <- io getCommands
  mkXPrompt myShellPrompt c (getShellCompl cmds $ searchPredicate c) spawnHere

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask                , xK_Return     ), spawnHere $ XMonad.terminal conf)

    -- launch dmenu
    , ((modMask                , xK_p          ), runMyShellPrompt basicPrompt)

    -- launch gmrun
    , ((modMask .|. shiftMask  , xK_p          ), spawnHere "gmrun")

    -- change xmonad working directory
    , ((modMask                , xK_equal      ), changeDir def)

    , ((modMask                , xK_backslash  ), spawnHere "$BROWSER")

    -- Lock the screen.
    , ((modMask .|. shiftMask  , xK_z          ), spawn "i3lock -c 002b36")

    -- Volume controls
    , ((modMask                , xK_F6         ), spawn "$HOME/bin/volume -5%")
    , ((modMask                , xK_F7         ), spawn "$HOME/bin/volume +5%")
    , ((modMask .|. shiftMask  , xK_F6         ), spawn "mpc --host $HOME/.mpd/socket volume -5")
    , ((modMask .|. shiftMask  , xK_F7         ), spawn "mpc --host $HOME/.mpd/socket volume +5")
    , ((modMask                , xK_F8         ), spawn "$HOME/bin/volume toggle")

    -- Kill the screen backlight.
    , ((modMask                , xK_F5         ), spawn "sleep 1 ; xset dpms force off")

    -- Music controls
    , ((modMask                , xK_F3         ), spawn "mpc-prev")
    , ((modMask                , xK_F4         ), spawn "mpc-next")
    , ((modMask .|. shiftMask  , xK_F3         ), spawn "mpc-prev-remote")
    , ((modMask .|. shiftMask  , xK_F4         ), spawn "mpc-next-remote")
    , ((modMask                , xK_F5         ), spawn "mpc --host $HOME/.mpd/socket toggle")
    , ((modMask .|. shiftMask  , xK_F5         ), spawn "mpc-toggle")
    , ((modMask                , xK_Scroll_Lock), spawn "nowplaying.sh")

    -- close focused window; only deletes a copy
    , ((modMask .|. shiftMask  , xK_c          ), kill1)

    -- REALLY kill the focussed window; sends the WM_CLOSE event to the window
    , ((modMask .|. shiftMask  , xK_x          ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask                , xK_space      ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask  , xK_space      ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask                , xK_n          ), refresh)

    -- Move focus to the next window
    , ((modMask                , xK_Tab        ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask                , xK_j          ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask                , xK_k          ), windows W.focusUp  )

    -- Swap focussed window with master window.
    , ((modMask                , xK_m          ), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask  , xK_j          ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask  , xK_k          ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask                , xK_h          ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask                , xK_l          ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask                , xK_t          ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask                , xK_comma      ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask                , xK_period     ), sendMessage (IncMasterN (-1)))

    -- Restart xmonad
    , ((modMask                , xK_q          ), restart "xmonad" True)
    -- Quit xmonad
    , ((modMask .|. shiftMask  , xK_q          ), io exitSuccess)

    -- Workspace management
    , ((modMask                , xK_Left       ), prevWS)
    , ((modMask                , xK_Right      ), nextWS)
    , ((modMask .|. shiftMask  , xK_Left       ), shiftToPrev)
    , ((modMask .|. shiftMask  , xK_Right      ), shiftToNext)
    , ((modMask                , xK_Down       ), nextScreen)
    , ((modMask                , xK_Up         ), prevScreen)
    , ((modMask .|. shiftMask  , xK_Down       ), shiftNextScreen)
    , ((modMask .|. shiftMask  , xK_Up         ), shiftPrevScreen)
    , ((modMask                , xK_z          ), toggleWS)

    -- Dynamic workspace management
    , ((modMask .|. shiftMask  , xK_BackSpace  ), removeWorkspace)
    , ((modMask                , xK_b          ), selectWorkspace basicPrompt)
    , ((modMask .|. shiftMask  , xK_b          ), withWorkspace basicPrompt (windows . W.shift))
    , ((modMask .|. controlMask, xK_b          ), withWorkspace basicPrompt (windows . copy))
    , ((modMask                , xK_a          ), renameWorkspace basicPrompt)

    -- relative multihead movement
    , ((modMask .|. shiftMask   , xK_h          ), viewScreenLeft)
    , ((modMask .|. shiftMask   , xK_l          ), viewScreenRight)
    , ((modMask .|. shiftMask .|. controlMask  , xK_h          ), shiftScreenLeft)
    , ((modMask .|. shiftMask .|. controlMask  , xK_l          ), shiftScreenRight)
    ]

    --
    -- mod-shift-{h,l}, switch to {left,right} screen.
    -- mod-shift-control-{h,l}, move window to {left,right} screen.
    --
    ++
    [((m .|. modMask .|. shiftMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, controlMask)]]

-- | Transforms the windowset by performing an action on the workspace
-- of a screen relative to the currently focused one.
screenRelative :: Int -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
screenRelative k f = do
  -- enumerate all the windows, and then find the one we're currently in
  ws <- gets windowset
  let allScreens = W.screens ws
  let n = length allScreens
  let S currentScreenId = W.screen $ W.current ws
  case
    findIndex (currentScreenId ==) screenIds >>=
    \idx -> elemIndex (k + idx `mod` n) screenIds
    of
    Nothing -> pure ()
    Just newIdx ->
      screenWorkspace (S newIdx) >>= flip whenJust (windows . f)

viewScreenLeft = screenRelative (1) W.view
viewScreenRight = screenRelative (-1) W.view
shiftScreenLeft = screenRelative 1 W.shift
shiftScreenRight = screenRelative (-1) W.shift


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

myLayout = workspaceDir "~" $ smartBorders $ smartSpacing 5 $
            (avoidStrutsOn [U] $ Full ||| tiled)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1 / 1.608

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageSpawn <+> manageDocks <+> composeAll
    [ className =? "MPlayer"                              --> doFloat
    , className =? "Gimp"                                 --> doFloat
    , className =? "org-spoutcraft-launcher-Main"         --> doFloat
    , className =? "net-minecraft-MinecraftLauncher"      --> doFloat
    , className =? "net-ftb-mclauncher-MinecraftLauncher" --> doFloat
    , className =? "StepMania"                            --> doFloat
    , className =? "CaveStory+"                           --> doFloat
    , resource  =? "desktop_window"                       --> doIgnore
    ]

startStatusBar =
  pure (<>)
  <*> pure (statusBarGeneric myConkyCommand (pure ()))
  <*> statusBarPipe myDzenCommand (pure tsaniPP)
  where
    myDzenCommand =
      intercalate " "
      [ "dzen2"
      , "-dock", "-p", "-x 0"
      , "-w", show width
      , " -fn mononoki:size=10"
      ]
      where
        width = myScreenWidth * 3 `div` 5

    myConkyCommand = concat
        [ "conky | dzen2 -dock"
        , " -x ", show offset
        , " -w ", show width
        , " -ta r"
        ] where
            width = myScreenWidth * 2 `div` 5
            offset = myScreenWidth * 3 `div` 5

main :: IO ()
main = do
    -- w <- rect_width <$> getLeftmostScreenRect
    sb <- startStatusBar
    dirs <- getDirectories
    (`launch` dirs) . withSB sb . withUrgencyHook dzenUrgencyHook . ewmh . docks $ def
        -- simple stuff
        { terminal           = myTerminal
        , focusFollowsMouse  = True
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces

        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        -- key bindings
        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        -- hooks, layouts
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , startupHook        = killAllStatusBars
        }

-- Colors
solarizedBase03 = "#002b36"
solarizedBase02 = "#073642"
solarizedBase01 = "#586e75"
solarizedBase00 = "#657b83"
solarizedBase0 = "#839496"
solarizedBase1 = "#93a1a1"
solarizedBase2 = "#eee8d5"
solarizedBase3 = "#fdf6e3"
solarizedYellow = "#b58900"
solarizedOrange = "#cb4b16"
solarizedRed = "#dc322f"
solarizedMagenta = "#d33682"
solarizedViolet = "#6c71c4"
solarizedBlue = "#268bd2"
solarizedCyan = "#2aa198"
solarizedGreen = "#859900"

tsaniPP :: PP
tsaniPP =
  def
  { ppHiddenNoWindows = const ""
  , ppHidden  = dzenColor fgMain bg . pad
  , ppCurrent = dzenColor solarizedRed bg . pad
  , ppUrgent  = dzenColor solarizedMagenta bg . pad
  , ppSep     = dzenColor fgMain bg $ pad "|"
  , ppVisible = dzenColor solarizedBlue bg . pad
  , ppWsSep   = ""
  , ppLayout  = const ""
  , ppTitle   = dzenColor fgMain bg . shorten 70
  , ppOrder   = reverse
  }
  where
    bg = solarizedBase03
    fgMain = solarizedBase0
    fgSecondary = solarizedBase01
    fgEmph = solarizedBase1
