{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

-- This was when we were using the generic "Offchip" method:
-- {- LANGUAGE ConstraintKinds, TypeFamilies -}
-- , FlexibleContexts, MultiParamTypeClasses

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
 
    -- * Par-monad operations, including GPU operations    
    ParAccelerate(..), 
    -- unsafeHybridVector, unsafeHybridIArray,
--    module AC,     
--    module OC, 
    module PC    
 ) where

-- import Data.Array.Accelerate (Acc,Arrays)
-- import Data.Array.Accelerate.CUDA (Async)
import Data.Monoid
import Control.Monad.Par.Class as PC
import Control.Monad.Par.Accelerate as AC
-- import qualified Control.Monad.Par.OffChip    as OC
import qualified Control.Monad.Par.Meta as Meta 
import qualified Control.Monad.Par.Meta.Resources.Accelerate as Rsrc
import qualified Control.Monad.Par.Meta.Resources.SMP as SMP
import GHC.Conc (numCapabilities)

--------------------------------------------------------------------------------

tries :: Int
tries = numCapabilities

newtype Par a = AccSMPPar (Meta.Par a)
 deriving (Monad, Functor, 
           PC.ParFuture     Meta.IVar,
           PC.ParIVar       Meta.IVar,
           AC.ParAccelerate Meta.IVar 
          ) -- NOT MonadIO  

resource :: Meta.Resource
resource = SMP.mkResource tries `mappend` Rsrc.mkResource

-- <boilerplate>
{-
instance OC.ParOffChip Acc Meta.IVar Par where 
  type OffChipConstraint a = Arrays a 
  runOffChip   r x = AccSMPPar$ Rsrc.runAcc   r x
  spawnOffChip r x = AccSMPPar$ Rsrc.spawnAcc r x
  unsafeHybrid r cvt (AccSMPPar p,a) = AccSMPPar$ Rsrc.unsafeHybrid r cvt (p,a)  
-}

-- run1Async :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> Async b


{-
instance OC.ParOffChip Async Meta.IVar Par where 
  type OffChipConstraint a = Arrays a 
  runOffChip   r x = AccSMPPar$ Rsrc.runAcc   r x
  spawnOffChip r x = AccSMPPar$ Rsrc.spawnAcc r x
  unsafeHybrid r cvt (AccSMPPar p,a) = AccSMPPar$ Rsrc.unsafeHybrid r cvt (p,a)  
-}

runPar   :: Par a -> a
runPar (AccSMPPar p) = Meta.runMetaPar resource p

runParIO :: Par a -> IO a
runParIO (AccSMPPar p) = Meta.runMetaParIO resource p
