{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE KindSignatures, ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -Wall #-}

-- | This module is conceptually part of "Control.Monad.Par.Class", but
--   is factored into a separate package because it depends on
--   ConstraintKinds, available only in GHC 7.4 and later.

module Control.Monad.Par.OffChip
       (
         -- * The Class
         ParOffChip(..),
       
         -- * Example applications of `unsafeHybrid`
---    unsafeHybridVector,
--     unsafeHybridIArray
       ) where 

import Control.Monad.Par.Class
-- import Data.Array.IArray (IArray)
-- import Foreign (Ptr, Storable)
-- import qualified Data.Array.IArray as IArray    
-- import qualified Data.Vector.Storable as Vector
import GHC.Exts (Constraint)

--------------------------------------------------------------------------------

-- | A generic interface for operations that happen outside of the CPU.
class ParFuture ivar m => ParOffChip con ivar m where 
  -- | A constraint on types that must be sent off of the CPU.  This
  -- typically includes, but is not limited to, serializability.  
  type OffChipConstraint a :: Constraint

  -- | Run an computation off of the CPU and wait for its result.  A
  -- common example is invoking a GPU computation.
  --   
  -- > runOffChip runner foreignComp 
  -- 
  -- From the type of this function you can see that, @runner@ /already/
  -- has the capability to execute the foreign computations.  The
  -- purpose of using runOffChip is to inform the 'Par' CPU scheduler
  -- that a blocking operation is about to occur.
  --     
  -- This can result in better performance by enabling the CPU to do
  -- other work while waiting for the off-chip computation to complete.
  runOffChip   :: (OffChipConstraint a) => (con a -> a) -> con a -> m a
  
  -- | Non-blocking variant of `runOffChip`.
  spawnOffChip :: (OffChipConstraint a) => (con a -> a) -> con a -> m (ivar a)
--  default spawnOffChip :: (ParIVar ivar m, C a) => (con a -> a) -> con a -> m (ivar a)
  spawnOffChip runner comp = spawn_ (runOffChip runner comp)

  -- | Spawn an computation which may execute /either/ on the CPU or
  -- off-chip based on runtime load.  The CPU and off-chip
  -- implementations may employ completely different algorithms.
  -- Therefore, this is an UNSAFE operation which will not guarantee
  -- determinism unless the user /ensures/ that the two algorithms are
  -- equivalent.
  --
  -- Usage is:  
  -- 
  -- > unsafeHybrid runner conversion (cpuVer, offChipVer)
  -- 
  -- As with `runOffChip`, the @runner@ invokes the actual off-chip
  -- computation.  The new parameter, @conversion@, converts results
  -- from the off-chip computation to be of the same type as the CPU
  -- version of the function.  Finally, the pair argument contains two
  -- complete computations, only one of which is invoked at runtime.
  -- 
  unsafeHybrid :: (OffChipConstraint b) => (con b -> b) -> (b -> a) -> (m a, con b) -> m (ivar a)

--------------------------------------------------------------------------------  

#if 0
-- | A class containing Accelerate-specific `Par` operations.
class ParFuture iv p => ParAccelerate iv p where 
  
  -- | Run an Accelerate computation and wait for its result.  In the
  -- context of a `Par` computation this can result in better
  -- performance than using an Accelerate-provided `run` function
  -- directly, because this version enables the CPU work scheduler to do
  -- other work while waiting for the GPU computation to complete.
  -- 
  -- Moreover, when configured with a high-performance /CPU/ Accelerate backend
  -- in the future this routine can enable automatic CPU/GPU work partitioning.
  runAcc   :: (Arrays a) => (Acc a -> a) -> Acc a -> p a
  runAcc comp = spawnAcc comp >>= get

  -- | Like `runAcc` but runs the Accelerate computation asynchronously.
  spawnAcc :: (Arrays a) => Acc a -> p (iv a)
  
  -- | Spawn an computation which may execute /either/ on the CPU or GPU
  -- based on runtime load.  The CPU and GPU implementations may employ
  -- completely different algorithms; this is an UNSAFE operation which
  -- will not guarantee determinism unless the user ensures that the
  -- result of both computations is always equivalent.
  -- 
  --     
  -- A common application of `unsafeHybrid` is the following:
  --
  -- > unsafeHybrid Data.Array.Accelerate.IO.toVector
  --
  unsafeHybrid :: Arrays b => (b -> a) -> (p a, Acc b) -> p (iv a)

--------------------------------------------------------------------------------

-- | An example application of `unsafeHybrid` for vectors.
unsafeHybridVector :: (Vector.Storable a, Elt a, 
                       IO.BlockPtrs (EltRepr a) ~ ((), Ptr a), 
                       ParAccelerate iv p)
                  => (p (Vector.Vector a), Acc (Array DIM1 a))
                  -> p (iv (Vector.Vector a))
-- /TODO/: make a variant with unrestricted 'Shape' that, e.g., yields
-- a vector in row-major order.
unsafeHybridVector = unsafeHybrid IO.toVector


-- | An example application of `unsafeHybrid` for any IArray type.
unsafeHybridIArray :: ( EltRepr ix ~ EltRepr sh
                     , IArray a e, IArray.Ix ix
                     , Shape sh, Elt ix, Elt e 
                     , ParAccelerate iv p)
                  => (p (a ix e), Acc (Array sh e))
                  ->  p (iv (a ix e))               
unsafeHybridIArray = unsafeHybrid toIArray
                     --IO.toArray 
#endif
