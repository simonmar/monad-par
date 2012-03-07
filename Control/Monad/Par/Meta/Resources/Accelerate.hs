{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Resources.Accelerate (
    defaultInit
  , defaultSteal
  , spawnAcc
  , spawnAccIArray
  , spawnAccVector
  , mkResource
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
import Data.Array.Accelerate.IO
import Data.Array.Accelerate.IO.Vector

import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray    

import qualified Data.Vector.Storable as Vector

import Data.Concurrent.Deque.Class (ConcQueue, WSDeque)
import Data.Concurrent.Deque.Reference as R

import Foreign (Ptr, Storable)

import System.IO.Unsafe

import Text.Printf

import Control.Monad.Par.Meta hiding (dbg)

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
-- spawnAcc operator and init/steal definitions to export

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
spawnAccVector :: (Storable a, Elt a, BlockPtrs (EltRepr a) ~ ((), Ptr a))
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

-- runAcc :: (Arrays a) => Acc a -> Par a


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

mkResource :: Resource
mkResource = Resource defaultInit defaultSteal

defaultInit :: InitAction
defaultInit = IA ia
  where ia _ _ = do
          void $ forkIO gpuDaemon

defaultSteal :: StealAction
defaultSteal = SA sa 
  where sa _ _ = do
          mfinished <- R.tryPopR resultQueue
          case mfinished of
            finished@(Just _) -> return finished
            Nothing -> fmap fst `fmap` R.tryPopL gpuBackstealQueue
    