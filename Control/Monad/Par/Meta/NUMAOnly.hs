{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.NUMAOnly (
    runPar
  , runParIO
  , module Control.Monad.Par.Meta
) where

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.NUMA as NUMA

tries :: Int
tries = 20

ia :: InitAction
ia = NUMA.initActionFromEnv

sa :: StealAction
sa = NUMA.stealActionFromEnv tries

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   ia sa
runParIO = runMetaParIO ia sa