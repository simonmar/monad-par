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

resource = mconcat [ SharedMemory.mkResource tries
                   , Merge.mkResource
                   , Bkoff.mkResource 1000 (100*1000)
                   ]

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   resource
runParIO = runMetaParIO resource