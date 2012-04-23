{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.AccSMP (
  -- * Running `Par` computations using Accelerate and SMP parallelism.
    Par
  , runPar
  , runParIO
  
--  , module Accelerate
  -- * Accelerate-specific `Par` operations:
  , Accelerate.runAcc
  , Accelerate.spawnAcc
  , Accelerate.unsafeHybrid    
  
  -- * Example applications of `unsafeHybrid`
  , Accelerate.unsafeHybridIArray
  , Accelerate.unsafeHybridVector
  
--  , module Control.Monad.Par.Meta
  , Meta.IVar
) where

import Data.Monoid
import qualified Control.Monad.Par.Class as PC
import qualified Control.Monad.Par.Meta as Meta 
import qualified Control.Monad.Par.Meta.Resources.Accelerate as Accelerate
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory
import GHC.Conc (numCapabilities)

tries :: Int
tries = numCapabilities

-- We use Constraint kinds just to flip the arguments here:
type ParFutureFlipped p = (PC.ParFuture p Meta.IVar)
                    
foo :: ParFutureFlipped p => p a -> Int
foo = undefined

-- | A `Par` monad supporting only SMP and GPU resources.
newtype Par a = AccSMPPar (Meta.Par a)
 deriving (Monad, Functor)
          -- NOT MonadIO

-- instance PC.ParFuture Par Meta.IVar where

resource :: Meta.Resource
resource = SharedMemory.mkResource tries <> Accelerate.mkResource

runPar   :: Meta.Par a -> a
runParIO :: Meta.Par a -> IO a
runPar   = Meta.runMetaPar   resource
runParIO = Meta.runMetaParIO resource