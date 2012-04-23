{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}

-- | A meta-par /scheduler/ for programming with shared memory
--   multicore CPUs plus GPU parallelism.
-- 
-- This provides a full implementation of the
-- 'Control.Monad.Par.Par'-monad programming abstraction
-- (specifically, the 'Control.Monad.Par.ParIVar' class).
--
module Control.Monad.Par.Meta.AccSMP 
 (
    Par, Meta.IVar,
    runPar, runParIO
 ) where

import Data.Monoid
import qualified Control.Monad.Par.Class as PC
import qualified Control.Monad.Par.Accelerate as AC
import qualified Control.Monad.Par.Meta as Meta 
import qualified Control.Monad.Par.Meta.Resources.Accelerate as Accelerate
import qualified Control.Monad.Par.Meta.Resources.SMP as SMP
import GHC.Conc (numCapabilities)

--------------------------------------------------------------------------------

tries :: Int
tries = numCapabilities

-- | A `Par` monad supporting only SMP and GPU resources.
newtype Par a = AccSMPPar (Meta.Par a)
 deriving (Monad, Functor, 
           PC.ParFuture     Meta.IVar,
           PC.ParIVar       Meta.IVar,
           AC.ParAccelerate Meta.IVar 
          )
          -- NOT MonadIO

resource :: Meta.Resource
resource = SMP.mkResource tries <> Accelerate.mkResource

runPar   :: Meta.Par a -> a
runParIO :: Meta.Par a -> IO a

-- | Running `Par` computations on CPU and GPU.
runPar   = Meta.runMetaPar   resource

-- | Same as `runPar` but don't hide the IO.
runParIO = Meta.runMetaParIO resource
