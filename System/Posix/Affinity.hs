{-# LANGUAGE ForeignFunctionInterface #-}

module System.Posix.Affinity (setAffinityOS) where

import Control.Monad
import Foreign.C
import System.IO.Error

foreign import ccall "pin_pthread" pin_pthread :: CInt -> IO Errno

setAffinityOS :: Int -> IO ()
setAffinityOS cpu = do
  err <- pin_pthread $ fromIntegral cpu
  when (err /= eOK) $ 
    ioError $ errnoToIOError "setAffinityOS" err Nothing Nothing