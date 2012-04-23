{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Accelerate 
       (
         -- * The Class
         ParAccelerate(..),
       
         -- * Example applications of `unsafeHybrid`
         unsafeHybridVector
       ) where 

import Control.Monad.Par.Class
import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray    
import qualified Data.Vector.Storable as Vector
import Data.Array.Accelerate (Acc, Arrays, Shape)
import Data.Array.Accelerate.Array.Sugar (EltRepr,Elt,Array,DIM1,toIArray)
import qualified Data.Array.Accelerate.IO as IO 
import Foreign (Ptr, Storable)

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
  runAcc   :: (Arrays a) => Acc a -> p a
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
