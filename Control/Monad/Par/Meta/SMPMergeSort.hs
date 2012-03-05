{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.SMPMergeSort (
    runPar
  , runParIO
  , Merge.blockingGPUMergeSort
  , Merge.spawnGPUMergeSort
  , Merge.spawnCPUGPUMergeSort
  , module Control.Monad.Par.Meta
) where

import Data.Monoid

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.CUDAMergeSort as Merge
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory
import qualified Control.Monad.Par.Meta.Resources.Backoff as Bkoff

tries :: Int
tries = 20

{-# INLINE ia #-}
ia :: InitAction
ia = mconcat [ SharedMemory.initAction
             , Merge.initAction
             , Bkoff.initAction
             ]

-- ia = SharedMemory.initAction <> Merge.initAction

{-# INLINE sa #-}
sa :: StealAction
sa = mconcat [ SharedMemory.stealAction tries
             , Merge.stealAction
             , Bkoff.mkStealAction 1000 (100*1000)
             ]
-- sa = SharedMemory.stealAction tries <> Merge.stealAction

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   ia sa
runParIO = runMetaParIO ia sa