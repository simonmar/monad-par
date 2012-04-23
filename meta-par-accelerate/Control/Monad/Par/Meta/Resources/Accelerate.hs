{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK ignore-exports, prune #-}

-- | Do not use his module directly.  Use a /SCHEDULER/ module (such
--   as `Control.Monad.Par.Meta.AccSMP`).
-- 
-- This provides a component (Resource) for assembling schedulers, as well as 
-- exporting a `Control.Monad.Par.Accelerate.ParAccelerate` instance.

module Control.Monad.Par.Meta.Resources.Accelerate 
  (
      mkResource
  ) where

import Control.Concurrent
import Control.Exception.Base (evaluate)
import Control.Monad
import Control.Monad.IO.Class

import Data.Array.Accelerate (Acc, Arrays)
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA as Acc
#else
import qualified Data.Array.Accelerate.Interpreter as Acc
#endif

import Data.Concurrent.Deque.Class (ConcQueue, WSDeque)
import Data.Concurrent.Deque.Reference as R

import System.IO.Unsafe

import Text.Printf

import qualified Control.Monad.Par.Accelerate as AC
import Control.Monad.Par.Meta 
import Control.Monad.Par.Class (new,put_)

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif


--------------------------------------------------------------------------------
-- * The `Resource` itself:

-- | A mix-in component for assembling schedulers with an Accelerate capability.
mkResource :: Resource
mkResource = Resource defaultInit defaultSteal


-- * /Internal/ Definitions
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

-- | See documentation for `Control.Monad.Par.Accelerate.spawnAcc`
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


-- | See documentation for `Control.Monad.Par.Accelerate.unsafeHybrid`
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

--------------------------------------------------------------------------------

-- Generic instance for Meta.Par, needs to be newtype-derived for specific schedulers.
instance AC.ParAccelerate IVar Par where 
  spawnAcc     = spawnAcc
  unsafeHybrid = unsafeHybrid
