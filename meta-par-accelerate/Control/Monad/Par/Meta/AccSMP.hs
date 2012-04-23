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
import qualified Control.Monad.Par.Meta.Resources.SMP as SMP
import GHC.Conc (numCapabilities)

tries :: Int
tries = numCapabilities

-- | A `Par` monad supporting only SMP and GPU resources.
newtype Par a = AccSMPPar (Meta.Par a)
 deriving (Monad, Functor, 
           PC.ParFuture Meta.IVar,
           PC.ParIVar   Meta.IVar 
          )
          -- NOT MonadIO

resource :: Meta.Resource
resource = SMP.mkResource tries <> Accelerate.mkResource

runPar   :: Meta.Par a -> a
runParIO :: Meta.Par a -> IO a
runPar   = Meta.runMetaPar   resource
runParIO = Meta.runMetaParIO resource