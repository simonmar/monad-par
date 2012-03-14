{-# LANGUAGE ForeignFunctionInterface #-}

module System.Posix.Affinity (setAffinityOS, setAffinityRange) where

import Control.Monad
import Foreign
import Foreign.C
import Foreign.Marshal.Array
import System.IO.Error

foreign import ccall "pin_pthread" pin_pthread :: CInt -> IO Errno
foreign import ccall "pin_process_multi" pin_process_multi :: Ptr CInt -> CInt -> IO Errno

setAffinityOS :: Int -> IO ()
setAffinityOS cpu = do
  err <- pin_pthread $ fromIntegral cpu
  when (err /= eOK) $ 
    ioError $ errnoToIOError "setAffinityOS" err Nothing Nothing

setAffinityRange :: [Int] -> IO ()
setAffinityRange cpus =
  withArrayLen cpus $ \len ptr -> do
    err <- pin_process_multi (castPtr ptr) (fromIntegral len)
    when (err /= eOK) $ 
      ioError $ errnoToIOError "setAffinityRangeOS" err Nothing Nothing