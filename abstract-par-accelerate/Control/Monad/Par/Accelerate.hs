{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Accelerate 
       (
         -- * The Class
         ParAccelerate(..),
       
#ifdef ACC_IO         
         -- * Example applications of `unsafeHybrid`
         unsafeHybridVector,
         unsafeHybridIArray
#endif
       ) where 

import Control.Monad.Par.Class
import Data.Array.IArray (IArray)
import Foreign (Ptr, Storable)
import qualified Data.Array.IArray as IArray    
import qualified Data.Vector.Storable as Vector

-- From 'accelerate':
import Data.Array.Accelerate (Acc, Arrays, Shape)
import Data.Array.Accelerate.Array.Sugar (EltRepr,Elt,Array,DIM1,toIArray)

#ifdef ACC_IO         
-- From 'accelerate-io', or 'accelerate" <= 0.10
import qualified Data.Array.Accelerate.IO as IO 
#endif

--------------------------------------------------------------------------------

-- | A class containing Accelerate-specific `Par` operations.
-- 
-- A minimal complete instance contains:
--  * one of `runAccWith` or `spawnAccWith`
--  * `getDefaultAccImpl`
--  * `compileAcc`.
class ParFuture iv p => ParAccelerate iv p where 
  
  -- | Run an Accelerate computation and wait for its result.  In the
  -- context of a `Par` computation this can result in better
  -- performance than using an Accelerate-provided `run` function
  -- directly, because this version enables the CPU work scheduler to do
  -- other work while waiting for the GPU computation to complete.
  -- 
  -- Moreover, when configured with a high-performance /CPU/ Accelerate backend
  -- in the future this routine can enable automatic CPU/GPU work partitioning.
  -- 
  -- The specific Accelerate implementation is NOT specified when
  -- calling `runAcc`.  That choice is deferred to the point where
  -- `runPar` is invoked for the scheduler in question.
  runAcc   :: (Arrays a) => Acc a -> p a
  runAcc comp = do runner <- getDefaultAccImpl
                   runAccWith runner comp

  -- | Like `runAcc` but runs the Accelerate computation asynchronously.
  spawnAcc :: (Arrays a) => Acc a -> p (iv a)
  spawnAcc comp = do runner <- getDefaultAccImpl
                     spawnAccWith runner comp

  
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
  unsafeHybrid cvrt pr = do runner <- getDefaultAccImpl 
                            unsafeHybridWith runner cvrt pr
                                           
  ------------------------------------------------------------
  -- * Control over selecting the Accelerate implementation.

  -- Retrieve the Accelerate @run@ function that is the default for
  -- this execution, i.e. the one used for `runAcc` or `spawnAcc`.
  getDefaultAccImpl :: Arrays a => p (Acc a -> a)

  -- | Like `runAcc` but specify a specific Accelerate implementation, e.g. @CUDA.run@.
  runAccWith   :: (Arrays a) => (Acc a -> a) -> Acc a -> p a
  runAccWith runner comp = spawnAccWith runner comp >>= get  
  
  -- | Analogous to `runAccWith`.
  spawnAccWith :: (Arrays a) => (Acc a -> a) -> Acc a -> p (iv a)
  -- This default implementation is actually QUITE BAD.  It's an
  -- anti-pattern.  We don't want to wait until the spawned
  -- computation is executed to enqueue the GPU computation.  This is
  -- a problem with child-stealing Par implemenations, but not so much
  -- with parent-stealing ones.
  spawnAccWith runner acc = spawn_ $ runAccWith runner  acc

  -- | Analogous to other @*With@ functions.
  unsafeHybridWith :: Arrays b => (Acc b -> b) -> (b -> a) -> (p a, Acc b) -> p (iv a)
  -- This default implementation simply /always/ runs the GPU version:
  unsafeHybridWith runner cvrt (_, acc) = 
    spawn_ $ do x <- runAccWith runner acc
                return (cvrt x)

  -- TODO: to be fully consistent we should perhaps have
  -- compileAccWith, but that gets complicated.

  ------------------------------------------------------------
  -- TODO: We would really like to add this, but it requires more than
  -- getDefaultAccImpl can provide right now.
#if 0
  -- | Prepare a GPU computation for repeated execution.  
  -- 
  -- Typically, this is applied to its first argument once in an outer
  -- scope then applied to its second argument repeatedly inside a loop.
  -- 
  -- Whereas the normal `runAcc` will /attempt/ to cache compiled
  -- programs and avoid recompilation, this function guarantees no
  -- recompilation and further avoids some overhead from re-executing
  -- the Accelerate front-end.
  -- 
  -- See "Data.Array.Accelerate.CUDA.run1" for more explanation.
  compileAcc :: (Arrays a, Arrays b) => (Acc a -> Acc b) -> a -> p b
#endif



--------------------------------------------------------------------------------

#ifdef ACC_IO         
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