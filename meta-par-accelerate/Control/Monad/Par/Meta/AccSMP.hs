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
    -- | A `Par` monad supporting only SMP and GPU resources.
    Par, 

    -- | The IVar type for use with the `Par` newtype in this module.
    Meta.IVar,

    -- | Running `Par` computations on CPU and GPU.
    runPar, 

    -- | Same as `runPar` but don't hide the IO.
    runParIO,
 
    -- Reexport the ParAccelerate class:
--    module Control.Monad.Par.Accelerate
    AC.ParAccelerate

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

-- timeit :: IO () -> IO Double
-- timeit act = do 
--   start <- getCurrentTime
-- printf "Creating vector took %0.3f sec.\n"
--             ((fromRational$ toRational $ diffUTCTime end start) :: Double)


newtype Par a = AccSMPPar (Meta.Par a)
 deriving (Monad, Functor, 
           PC.ParFuture     Meta.IVar,
           PC.ParIVar       Meta.IVar,
           AC.ParAccelerate Meta.IVar 
          )
          -- NOT MonadIO

resource :: Meta.Resource
resource = SMP.mkResource tries `mappend` Accelerate.mkResource

runPar   :: Meta.Par a -> a
runParIO :: Meta.Par a -> IO a

runPar   = Meta.runMetaPar   resource

runParIO = Meta.runMetaParIO resource
