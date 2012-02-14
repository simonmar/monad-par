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

ia :: InitAction
ia = SharedMemory.initAction

sa :: StealAction
sa = SharedMemory.stealAction tries

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   ia sa
runParIO = runMetaParIO ia sa