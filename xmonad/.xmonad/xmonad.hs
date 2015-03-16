import XMonad
import XMonad.Core
import XMonad.Actions.CycleWS
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.NoBorders
import System.IO(hPutStrLn)
import System.Exit
import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Hooks.EwmhDesktops

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = [ "1-chat", "2-web", "3-code", "4-misc", "5-misc"
                  , "6-misc", "7-games", "8-email", "9-music" ]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#DD0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- launch a terminal
    [ ((modMask,               xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask .|. shiftMask, xK_Return), spawn "urxvt -fg green -e sudo su -")

    -- launch dmenu
    , ((modMask,               xK_p     ), spawn "dmenu_run")

    -- launch gmrun
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modMask .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modMask,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modMask,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    -- , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    -- , ((modMask,               xK_Up), windows W.swapMaster)
    , ((modMask,               xK_m     ), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modMask,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- toggle the status bar gap
    -- TODO, update this binding with avoidStruts , ((modMask              , xK_b     ),

    -- Quit xmonad
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True)

    , ((modMask              , xK_Left       ), prevWS)
    , ((modMask              , xK_Right      ), nextWS)
    , ((modMask .|. shiftMask, xK_Left       ), shiftToPrev)
    , ((modMask .|. shiftMask, xK_Right      ), shiftToNext)
    , ((modMask              , xK_Down       ), nextScreen)
    , ((modMask              , xK_Up         ), prevScreen)
    , ((modMask .|. shiftMask, xK_Down       ), shiftNextScreen)
    , ((modMask .|. shiftMask, xK_Up         ), shiftPrevScreen)
    , ((modMask              , xK_z          ), toggleWS)
    , ((modMask .|. shiftMask, xK_z          ), spawn "xscreensaver-command -lock")
	, ((modMask              , xK_F9         ), spawn "volume toggle")
	, ((modMask              , xK_F10        ), spawn "volume down")
	, ((modMask              , xK_F11        ), spawn "volume up")
	, ((modMask              , xK_F5         ), spawn "sleep 1 ; xset dpms force off")
	, ((modMask              , xK_F6         ), spawn "xbacklight -dec 10")
	, ((modMask              , xK_F7         ), spawn "xbacklight -inc 10")
	, ((modMask              , xK_Page_Up    ), spawn "mpc prev")
	, ((modMask              , xK_Page_Down  ), spawn "mpc next")
	, ((modMask              , xK_Pause      ), spawn "mpc toggle")
    , ((modMask              , xK_Scroll_Lock), spawn "/home/tsani/bin/nowplaying.sh")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


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

phi = toRational $ (1 + sqrt 5) / 2

myLayout = smartBorders $ smartSpacing 5 $ Full |||
               (avoidStruts $ tiled ||| Mirror tiled ||| spiral phi ) -- -}
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 2/3

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

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
myManageHook = manageDocks <+> composeAll
    [ className =? "MPlayer"                              --> doFloat
    , className =? "Gimp"                                 --> doFloat
	, className =? "org-spoutcraft-launcher-Main"         --> doFloat
	, className =? "net-minecraft-MinecraftLauncher"      --> doFloat
	, className =? "net-ftb-mclauncher-MinecraftLauncher" --> doFloat
	, className =? "StepMania"                            --> doFloat
	, className =? "CaveStory+"                           --> doFloat
    , resource  =? "desktop_window"                       --> doIgnore
	]

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()

myLogHook h = dynamicLogWithPP dzenPP { ppOutput = hPutStrLn h }

myUrgencyHook = dzenUrgencyHook

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    myDzen <- spawnPipe "dzen2 -p"
    xmonad
      $ withUrgencyHook NoUrgencyHook
        $ ewmh defaultConfig
        -- simple stuff
        { terminal           = myTerminal
        , focusFollowsMouse  = True
        , borderWidth        = myBorderWidth
        , modMask            = myModMask

        -- numlockMask        = myNumlockMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        -- key bindings
        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        -- hooks, layouts
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , logHook            = myLogHook myDzen
        , startupHook        = myStartupHook
    }
