{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

-- | Do not use his module directly.  Use a /SCHEDULER/ module.  This
-- only provides a component (Resource) for assembling schedulers.

module Control.Monad.Par.Meta.Resources.Accelerate (
  -- * The `Resource` itself:
      mkResource
--  , defaultInit
--  , defaultSteal
    
  -- * Accelerate-specific `Par` operations:
  , runAcc
  , spawnAcc
  , unsafeHybrid    
  
  -- * Example applications of `unsafeHybrid`
  , unsafeHybridIArray
  , unsafeHybridVector

  ) where

import Control.Concurrent
import Control.Exception.Base (evaluate)
import Control.Monad
import Control.Monad.IO.Class

import Data.Array.Accelerate (Acc, Arrays)
import Data.Array.Accelerate.Array.Sugar
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA as Acc
#else
import qualified Data.Array.Accelerate.Interpreter as Acc
#endif
import qualified Data.Array.Accelerate.IO as IO -- Now has toVector...
import Data.Array.Accelerate.IO.Vector (toVector)

import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray    

import qualified Data.Vector.Storable as Vector

import Data.Concurrent.Deque.Class (ConcQueue, WSDeque)
import Data.Concurrent.Deque.Reference as R

import Foreign (Ptr, Storable)

import System.IO.Unsafe

import Text.Printf

import Control.Monad.Par.Meta 
import Control.Monad.Par.Class (new, put_, get)

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

--------------------------------------------------------------------------------
-- Global structures for communicating between Par threads and GPU
-- daemon threads

{-# NOINLINE gpuOnlyQueue #-}
-- | GPU-only queue is pushed to by 'Par' workers on the right, and
-- popped by the GPU daemon on the left. No backstealing is possible
-- from this queue.
gpuOnlyQueue :: WSDeque (IO ())
gpuOnlyQueue = unsafePerformIO R.newQ

{-# NOINLINE gpuBackstealQueue #-}
-- | GPU-only queue is pushed to by 'Par' workers on the right, and
-- popped by the GPU daemon and 'Par' workers on the left.
gpuBackstealQueue :: ConcQueue (Par (), IO ())
gpuBackstealQueue = unsafePerformIO R.newQ

{-# NOINLINE resultQueue #-}
-- | Result queue is pushed to by the GPU daemon, and popped by the
-- 'Par' workers, meaning the 'WSDeque' is appropriate.
resultQueue :: WSDeque (Par ())
resultQueue = unsafePerformIO R.newQ

--------------------------------------------------------------------------------

-- | Run an Accelerate computation and wait for its result.  In the
-- context of a `Par` computation this can result in better
-- performance than using an Accelerate-provided `run` function
-- directly, because this version enables the CPU work scheduler to do
-- other work while waiting for the GPU computation to complete.
-- 
-- Moreover, when configured with a high-performance /CPU/ Accelerate backend
-- in the future this routine can enable automatic CPU/GPU work partitioning.
runAcc :: (Arrays a) => Acc a -> Par a
runAcc comp = spawnAcc comp >>= get

----------------------------------------

-- | Like `runAcc` but runs the Accelerate computation asynchronously.
spawnAcc :: (Arrays a) => Acc a -> Par (IVar a)
spawnAcc comp = do 
    when dbg $ liftIO $ printf "spawning Accelerate computation\n"
    iv <- new
    let wrappedComp = do
          when dbg $ printf "running Accelerate computation\n"
          ans <- evaluate $ Acc.run comp
          R.pushL resultQueue $ do
            when dbg $ liftIO $ printf "Accelerate computation finished\n"
            put_ iv ans
    liftIO $ R.pushR gpuOnlyQueue wrappedComp
    return iv               

-- Backstealing variants

#if 0
-- | Backstealing spawn where the result is converted to an instance
-- of 'IArray'. Since the result of either 'Par' or the 'Acc' version
-- may be put in the resulting 'IVar', it is expected that the result
-- of both computations is an equivalent 'IArray'.
spawnAccIArray :: ( EltRepr ix ~ EltRepr sh
                  , IArray a e, IArray.Ix ix
                  , Shape sh, Elt ix, Elt e )
               => (Par (a ix e), Acc (Array sh e))
               -> Par (IVar (a ix e))               
spawnAccIArray (parComp, accComp) = do 
    when dbg $ liftIO $ printf "spawning Accelerate computation\n"
    iv <- new
    let wrappedParComp :: Par ()
        wrappedParComp = do
          when dbg $ liftIO $ printf "running backstolen computation\n"
          put_ iv =<< parComp          
        wrappedAccComp = do
          when dbg $ printf "running Accelerate computation\n"
          ans <- evaluate $ Acc.run accComp
          R.pushL resultQueue $ do
            when dbg $ liftIO $ printf "Accelerate computation finished\n"
            put_ iv (toIArray ans)
    liftIO $ R.pushR gpuBackstealQueue (wrappedParComp, wrappedAccComp)
    return iv

-- | Backstealing spawn where the result is converted to a
-- 'Data.Vector.Storable.Vector'. Since the result of either 'Par' or
-- the 'Acc' version may be put in the resulting 'IVar', it is
-- expected that the result of both computations is an equivalent
-- 'Vector'. /TODO/: make a variant with unrestricted 'Shape' that,
-- e.g., yields a vector in row-major order.
spawnAccVector :: (Storable a, Elt a, IO.BlockPtrs (EltRepr a) ~ ((), Ptr a))
               => (Par (Vector.Vector a), Acc (Array DIM1 a))
               -> Par (IVar (Vector.Vector a))
spawnAccVector (parComp, accComp) = do 
    when dbg $ liftIO $ printf "spawning Accelerate computation\n"
    iv <- new
    let wrappedParComp :: Par ()
        wrappedParComp = do
          when dbg $ liftIO $ printf "running backstolen computation\n"
          put_ iv =<< parComp
        wrappedAccComp :: IO ()
        wrappedAccComp = do
          when dbg $ printf "running Accelerate computation\n"
          ans <- toVector $ Acc.run accComp
          R.pushL resultQueue $ do
            when dbg $ liftIO $ printf "Accelerate computation finished\n"
            put_ iv ans
    liftIO $ R.pushR gpuBackstealQueue (wrappedParComp, wrappedAccComp)
    return iv
#endif


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
unsafeHybrid :: Arrays b => (b -> a) -> (Par a, Acc b) -> Par (IVar a)
unsafeHybrid convert (parComp, accComp) = do 
    when dbg $ liftIO $ printf "spawning Accelerate computation\n"
    iv <- new
    let wrappedParComp :: Par ()
        wrappedParComp = do
          when dbg $ liftIO $ printf "running backstolen computation\n"
          put_ iv =<< parComp
        wrappedAccComp :: IO ()
        wrappedAccComp = do
          when dbg $ printf "running Accelerate computation\n"
--          ans <- convert $ Acc.run accComp
          let ans = convert $ Acc.run accComp
          R.pushL resultQueue $ do
            when dbg $ liftIO $ printf "Accelerate computation finished\n"
            put_ iv ans
    liftIO $ R.pushR gpuBackstealQueue (wrappedParComp, wrappedAccComp)
    return iv

-- | An example application of `unsafeHybrid` for vectors.
unsafeHybridVector :: (Storable a, Elt a, IO.BlockPtrs (EltRepr a) ~ ((), Ptr a))
                  => (Par (Vector.Vector a), Acc (Array DIM1 a))
                  -> Par (IVar (Vector.Vector a))
-- /TODO/: make a variant with unrestricted 'Shape' that, e.g., yields
-- a vector in row-major order.
unsafeHybridVector = unsafeHybrid IO.toVector

-- | An example application of `unsafeHybrid` for any IArray type.
unsafeHybridIArray :: ( EltRepr ix ~ EltRepr sh
                     , IArray a e, IArray.Ix ix
                     , Shape sh, Elt ix, Elt e )
                  => (Par (a ix e), Acc (Array sh e))
                  -> Par (IVar (a ix e))               
unsafeHybridIArray = unsafeHybrid toIArray


--------------------------------------------------------------------------------

-- | Loop for the GPU daemon; repeatedly takes work off the 'gpuQueue'
-- and runs it.
gpuDaemon :: IO ()
gpuDaemon = do
  when dbg $ printf "gpu daemon entering loop\n" 
  mwork <- R.tryPopL gpuOnlyQueue
  case mwork of
    Just work -> work
    Nothing -> do
      mwork2 <- R.tryPopL gpuBackstealQueue
      case mwork2 of
        Just (_, work) -> work 
        Nothing -> return ()
  gpuDaemon

-- | A mix-in component for assembling schedulers
mkResource :: Resource
mkResource = Resource defaultInit defaultSteal

defaultInit :: Startup
defaultInit = St ia
  where ia _ _ = do
          void $ forkIO gpuDaemon

defaultSteal :: WorkSearch
defaultSteal = WS sa 
  where sa _ _ = do
          mfinished <- R.tryPopR resultQueue
          case mfinished of
            finished@(Just _) -> return finished
            Nothing -> fmap fst `fmap` R.tryPopL gpuBackstealQueue
