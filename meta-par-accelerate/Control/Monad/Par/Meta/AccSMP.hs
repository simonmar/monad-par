{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.AccSMP (
  -- * Running `Par` computations using Accelerate and SMP parallelism.
    runPar
  , runParIO
  
--  , module Accelerate
  -- * Accelerate-specific `Par` operations:
  , Accelerate.runAcc
  , Accelerate.spawnAcc
  , Accelerate.unsafeHybrid    
  
  -- * Example applications of `unsafeHybrid`
  , Accelerate.unsafeHybridIArray
  , Accelerate.unsafeHybridVector
  
  , module Control.Monad.Par.Meta
) where

-- TODO - NEWTYPE

import Data.Monoid

import Control.Monad.Par.Meta
import qualified Control.Monad.Par.Meta.Resources.Accelerate as Accelerate
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory

tries :: Int
tries = 20

resource = SharedMemory.mkResource tries <> Accelerate.mkResource

runPar   :: Par a -> a
runParIO :: Par a -> IO a
runPar   = runMetaPar   resource
runParIO = runMetaParIO resource