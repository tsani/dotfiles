#!/usr/bin/runhaskell

{-# LANGUAGE ScopedTypeVariables #-}

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.IO.Error (catchIOError)
import Foreign.C.Types (CLong(..))
import Foreign.C.String (peekCString, CString)
import Data.Maybe (catMaybes)
import Control.Monad (forM_)

surfUrlAtomName = "_SURF_URI"
titleAtomName = "WM_NAME"
pidAtomName = "_NET_WM_PID"

data WinInfo = WinInfo
    { winTitle :: String
    , winUrl :: String
    , winPid :: Int
    }

-- | Runs an IO action, catching any exceptions thrown and yielding Nothing in 
-- that case.
try :: IO a -> IO (Maybe a)
try a = fmap Just a `catchIOError` const (return Nothing)

-- | Safely get a text property from a window.
safeGetTextProperty :: Display -> Atom -> Window -> IO (Maybe String)
safeGetTextProperty d a w = traverse peekCString . fmap tp_value =<< try (getTextProperty d w a)

-- | Safely get a (32-bit) integer property from a window.
safeGetIntProperty :: Display -> Atom -> Window -> IO (Maybe Int)
safeGetIntProperty d a w = fmap (fromIntegral . head) <$> getWindowProperty32 d a w

-- | Chop a string past a certain limit and append a string showing truncation
-- If the string is not long enough to be truncated, then no end-string is 
-- appended.
truncString :: Int -- ^ Limit for truncation
            -> String -- ^ Padding, e.g. "..."
            -> String -- ^ String to truncate
            -> String -- ^ Result
truncString 0 p (x:xs) = p
truncString n p [] = []
truncString n p (x:xs) = x : truncString (n - 1) p xs

-- | Ensures that a string is at least a certain length long by padding it on
-- the right with a character.
rpad :: Int -- ^ Limit for padding
     -> Char -- ^ Character to pad with
     -> String -- ^ String to pad.
     -> String -- ^ Result
rpad 0 _ [] = []
rpad 0 _ xs = xs
rpad n c [] = replicate n c
rpad n c (x:xs) = x : rpad (n - 1) c xs

showWinInfo (WinInfo { winTitle = title
                     , winUrl = url
                     , winPid = pid
                     }
            ) = nPid ++ nUrl ++ nTitle
    where nPid = rpad 8 ' ' (show pid) ++ " |"
          nUrl = rpad 30 ' ' $ truncString 25 " ..." url
          nTitle = rpad 40 ' ' $ truncString 35 " ..." title

main = do
    -- Open the display given by the DISPLAY environment variable.
    d <- openDisplay ""

    -- Get the root window for screen zero.
    root <- rootWindow d 0

    -- List all open windows
    (_, _, ws) <- queryTree d root

    -- Register the atom with the X server, so we can query for its value in 
    -- windows later.
    surfUrlAtom <- internAtom d surfUrlAtomName False
    pidAtom <- internAtom d pidAtomName False
    titleAtom <- internAtom d titleAtomName False

    let safeGetSurfUrl = safeGetTextProperty d surfUrlAtom
    let safeGetPid = safeGetIntProperty d pidAtom
    let safeGetTitle = safeGetTextProperty d titleAtom

    -- Safely extract the surf URL from each window and cull the Nothing values
    winData <- fmap ( map showWinInfo
                    . catMaybes 
                    )
             . mapM (\w -> do
                 mUrl <- safeGetSurfUrl w
                 mPid <- safeGetPid w
                 mTitle <- safeGetTitle w
                 return $ do
                    url <- mUrl
                    pid <- mPid
                    title <- mTitle
                    return $ WinInfo
                        { winTitle = title
                        , winUrl = url
                        , winPid = pid
                        }
             )
             $ ws

    forM_ winData putStrLn
    return ()
