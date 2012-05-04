{-# LANGUAGE GeneralizedNewtypeDeriving, ConstraintKinds, FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
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
 
--    module AC, 
    module OC, 
    module PC    
 ) where

import Data.Array.Accelerate (Acc,Arrays)
import Data.Monoid
import Control.Monad.Par.Class as PC
import qualified Control.Monad.Par.OffChip    as OC
import qualified Control.Monad.Par.Meta as Meta 
import qualified Control.Monad.Par.Meta.Resources.Accelerate as Rsrc
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
           PC.ParIVar       Meta.IVar
--           OC.ParOffChip    Acc Meta.IVar 
--           AC.ParAccelerate Meta.IVar 
          )
          -- NOT MonadIO

-- runOffChip   :: (OffChipConstraint a) => (con a -> a) -> con a -> m a

instance OC.ParOffChip Acc Meta.IVar Par where 
  type OffChipConstraint a = Arrays a 
  runOffChip   r x = AccSMPPar$ Rsrc.runAcc r x
--   spawnOffChip = spawnAcc
--   unsafeHybrid = unsafeHybrid

  -- instance OC.ParOffChip Acc IVar Par where

--   runOffChip = runAcc
--   spawnOffChip = spawnAcc
--   unsafeHybrid = unsafeHybrid

  
resource :: Meta.Resource
resource = SMP.mkResource tries `mappend` Rsrc.mkResource

runPar   :: Meta.Par a -> a
runParIO :: Meta.Par a -> IO a

runPar   = Meta.runMetaPar   resource

runParIO = Meta.runMetaParIO resource
