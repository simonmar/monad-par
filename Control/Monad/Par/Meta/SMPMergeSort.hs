{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.SMPMergeSort (
    runPar
  , runParIO
  , Merge.spawnMergeSort
  , Merge.unsafeSpawnMergeSort
  , module Control.Monad.Par.Meta
) where

import Data.Monoid

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.CUDAMergeSort as Merge
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory

tries :: Int
tries = 20

ia :: InitAction
ia = SharedMemory.initAction <> Merge.initAction

sa :: StealAction
sa = SharedMemory.stealAction tries <> Merge.stealAction

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   ia sa
runParIO = runMetaParIO ia sa