{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Resources.CUDAMergeSort (
    initAction
  , stealAction
  , spawnMergeSort
  , unsafeSpawnMergeSort
) where

import Control.Concurrent
import Control.Exception.Base (evaluate)
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Vector.Algorithms.Merge as VM
import Data.Vector.Algorithms.CUDA.Merge
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

import Data.Concurrent.Deque.Class (ConcQueue, WSDeque)
import Data.Concurrent.Deque.Reference as R

import Data.Word (Word32)

import System.IO.Unsafe

import Text.Printf

import Control.Monad.Par.Meta hiding (dbg, stealAction)
import Control.Monad.Par.Meta.HotVar.IORef

dbg :: Bool
-- #define DEBUG
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

--------------------------------------------------------------------------------
-- Global structures for communicating between Par threads and GPU
-- daemon thread

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

{-# NOINLINE daemonTid #-}
daemonTid :: HotVar (Maybe ThreadId)
daemonTid = unsafePerformIO $ newHotVar Nothing

--------------------------------------------------------------------------------
-- spawnMergeSort operator and init/steal definitions to export

spawnMergeSort :: V.Vector Word32 -> Par (IVar (V.Vector Word32))
spawnMergeSort v = spawnMergeSort' mergeSort v

unsafeSpawnMergeSort :: V.Vector Word32 -> Par (IVar (V.Vector Word32))
unsafeSpawnMergeSort v = spawnMergeSort' unsafeMergeSort v

{-# INLINE spawnMergeSort' #-}
spawnMergeSort' :: (V.Vector Word32 -> IO (V.Vector Word32))
                -> V.Vector Word32
                -> Par (IVar (V.Vector Word32))
spawnMergeSort' ms v = do 
    when dbg $ liftIO $ printf "spawning CUDA mergesort computation\n"
    iv <- new
    let wrappedCUDAComp = do
          when dbg $ printf "running CUDA mergesort computation\n"
          ans <- ms v
          R.pushL resultQueue $ do
            when dbg $ liftIO $ printf "CUDA mergesort computation finished\n"
            put_ iv ans          
        wrappedParComp = do
          when dbg $ liftIO $ printf "running backstolen computation\n"
          put_ iv =<< parMergeSort v          
    liftIO $ R.pushR gpuBackstealQueue (wrappedParComp, wrappedCUDAComp)
    return iv     

-- TODO: configure with different bottoming-out sorts
parMergeSort :: V.Vector Word32 -> Par (V.Vector Word32)
parMergeSort v = liftIO $ do
  mv <- V.thaw v
  VM.sort (mv :: M.IOVector Word32)
  V.unsafeFreeze mv

-- | Loop for the GPU daemon; repeatedly takes work off the 'gpuQueue'
-- and runs it.
gpuDaemon :: IO ()
gpuDaemon = do
  when dbg $ printf "gpu daemon entering loop\n" 
  mwork <- R.tryPopL gpuBackstealQueue
  case mwork of
    Just (_, work) -> work >> gpuDaemon
    Nothing -> gpuDaemon

initAction :: InitAction
initAction = IA ia
  where ia _ _ = do
          mtid <- readHotVar daemonTid
          when (mtid == Nothing)
            $ writeHotVar daemonTid . Just =<< forkIO gpuDaemon

#define GPU_BACKSTEALING
#ifdef GPU_BACKSTEALING
stealAction :: StealAction
stealAction = SA sa 
  where sa _ _ = do
          mfinished <- R.tryPopR resultQueue
          case mfinished of
            finished@(Just _) -> return finished
            Nothing -> fmap fst `fmap` R.tryPopL gpuBackstealQueue        
#else
stealAction :: StealAction
stealAction = SA sa 
  where sa _ _ = R.tryPopR resultQueue
#endif