#!/usr/bin/env runhaskell

module Main where

import qualified System.INotify as I
import System.Environment ( getArgs )
import System.Process ( proc, createProcess )
import Control.Applicative ( (<$>) )
import Data.Time

handler tz prog args (I.Modified False _) = do
        t <- utcToLocalTime tz <$> getCurrentTime
        putStrLn . (++ " -- recompiling.") . show $ t
        createProcess (proc prog args)
        return ()
handler _ _ _ _ = putStrLn "Useless event."

main = do
    tz <- getCurrentTimeZone
    (watchPath:prog:args) <- getArgs
    notify <- I.initINotify
    wd <- I.addWatch notify [I.Modify] watchPath $ handler tz prog args
    getLine -- block the terminal while our watcher runs
    I.removeWatch wd
    return ()
