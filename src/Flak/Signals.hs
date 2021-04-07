module Flak.Signals (setKeyboardInterruptHandler) where

import Control.Monad
import System.Posix.Signals

setKeyboardInterruptHandler :: IO () -> IO ()
setKeyboardInterruptHandler h = do
    mask <- getSignalMask
    let handler = Catch h
    void $ installHandler keyboardSignal handler (Just mask)
