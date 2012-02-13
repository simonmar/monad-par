{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Resources.Accelerate (
    initAction
  , stealAction
  , spawnAcc
  ) where

import Control.Concurrent
import Control.DeepSeq
import Control.Exception.Base (evaluate)
import Control.Monad
import Control.Monad.IO.Class

import Data.Array.Accelerate
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA as Acc
#else
import qualified Data.Array.Accelerate.Interpreter as Acc
#endif

import Data.Concurrent.Deque.Class (WSDeque)
import Data.Concurrent.Deque.Reference as R

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
-- daemon threads

{-# NOINLINE gpuQueue #-}
-- | GPU queue is pushed to by 'Par' workers, and popped by the GPU
-- daemon. TODO: figure out which Deque class is appropriate here.
gpuQueue :: Chan (IO ())
gpuQueue = unsafePerformIO newChan

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
    liftIO $ writeChan gpuQueue wrappedComp
    return iv               


-- runAcc :: (Arrays a) => Acc a -> Par a


-- | Loop for the GPU daemon; repeatedly takes work off the 'gpuQueue'
-- and runs it.
gpuDaemon :: IO ()
gpuDaemon = do
  when dbg $ printf "gpu daemon entering loop\n" 
  join (readChan gpuQueue) >> gpuDaemon

initAction :: InitAction
initAction _ _ = do
  void $ forkIO gpuDaemon

stealAction :: StealAction
stealAction _ _ = R.tryPopR resultQueue