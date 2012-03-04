{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Resources.CUDAMergeSort (
    initAction
  , stealAction
  , spawnMergeSort
) where

import Control.Concurrent
import Control.Exception.Base (evaluate)
import Control.Monad
import Control.Monad.IO.Class

import Data.Vector.Algorithms.CUDA.Merge
import qualified Data.Vector.Storable as V

import Data.Concurrent.Deque.Class (ConcQueue, WSDeque)
import Data.Concurrent.Deque.Reference as R

import Data.Word (Word32)

import System.IO.Unsafe

import Text.Printf

import Control.Monad.Par.Meta hiding (dbg, stealAction)

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

--------------------------------------------------------------------------------
-- Global structures for communicating between Par threads and GPU
-- daemon thread

{-# NOINLINE gpuOnlyQueue #-}
-- | GPU-only queue is pushed to by 'Par' workers on the right, and
-- popped by the GPU daemon on the left.
gpuOnlyQueue :: WSDeque (IO ())
gpuOnlyQueue = unsafePerformIO R.newQ

{-# NOINLINE resultQueue #-}
-- | Result queue is pushed to by the GPU daemon, and popped by the
-- 'Par' workers, meaning the 'WSDeque' is appropriate.
resultQueue :: WSDeque (Par ())
resultQueue = unsafePerformIO R.newQ

--------------------------------------------------------------------------------
-- spawnAcc operator and init/steal definitions to export

spawnMergeSort :: V.Vector Word32 -> Par (IVar (V.Vector Word32))
spawnMergeSort v = do 
    when dbg $ liftIO $ printf "spawning CUDA mergesort computation\n"
    iv <- new
    let wrappedComp = do
          when dbg $ printf "running CUDA mergesort computation\n"
          ans <- unsafeMergeSort v
          R.pushL resultQueue $ do
            when dbg $ liftIO $ printf "CUDA mergesort computation finished\n"
            put_ iv ans
    liftIO $ R.pushR gpuOnlyQueue wrappedComp
    return iv               

-- | Loop for the GPU daemon; repeatedly takes work off the 'gpuQueue'
-- and runs it.
gpuDaemon :: IO ()
gpuDaemon = do
  when dbg $ printf "gpu daemon entering loop\n" 
  mwork <- R.tryPopL gpuOnlyQueue
  case mwork of
    Just work -> work
    Nothing -> gpuDaemon

initAction :: InitAction
initAction = IA ia
  where ia _ _ = do
          void $ forkIO gpuDaemon

stealAction :: StealAction
stealAction = SA sa 
  where sa _ _ = R.tryPopR resultQueue
        