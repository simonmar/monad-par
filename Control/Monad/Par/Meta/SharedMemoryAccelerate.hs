{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.SharedMemoryAccelerate (
    runPar
  , runParIO
  , Accelerate.spawnAcc
  , module Control.Monad.Par.Meta
) where

import Data.Monoid

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.Accelerate as Accelerate
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory

tries :: Int
tries = 20

ia :: InitAction
ia = SharedMemory.initAction <> Accelerate.initAction

sa :: StealAction
sa = SharedMemory.stealAction tries <> Accelerate.stealAction

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   ia sa
runParIO = runMetaParIO ia sa