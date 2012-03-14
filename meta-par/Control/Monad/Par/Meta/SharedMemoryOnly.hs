{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.SharedMemoryOnly (
    runPar
  , runParIO
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory

tries :: Int
tries = 20

resource :: Resource
resource = SharedMemory.mkResource tries

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   resource
runParIO = runMetaParIO resource