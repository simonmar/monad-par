{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.SharedMemoryAccelerate (
    runPar
  , runParIO
  , Accelerate.spawnAcc
  , module Control.Monad.Par.Meta
) where

import Control.Monad

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.Accelerate as Accelerate
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory

tries :: Int
tries = 20

ia :: InitAction
ia = SharedMemory.initAction >> Accelerate.initAction

sa :: StealAction
sa sched schedRef = do
  mtask <- SharedMemory.stealAction tries sched schedRef
  case mtask of
    jtask@(Just _) -> return jtask
    Nothing -> Accelerate.stealAction sched schedRef

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   ia sa
runParIO = runMetaParIO ia sa