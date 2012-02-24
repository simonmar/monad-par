{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Resources.Accelerate (
    initAction
  , stealAction
  , spawnAcc
  , spawnAccIArray
  , spawnAccVector
  ) where

import Control.Concurrent
import Control.Exception.Base (evaluate)
import Control.Monad
import Control.Monad.IO.Class

import Data.Array.Accelerate
import Data.Array.Accelerate.Array.Sugar
#ifdef ACCELERATE_CUDA_BACKEND
import qualified Data.Array.Accelerate.CUDA as Acc
#else
import qualified Data.Array.Accelerate.Interpreter as Acc
#endif

import Data.Array.IArray (IArray)
import qualified Data.Array.IArray as IArray    

import qualified Data.Vector.Unboxed as Vector

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
spawnAccIArray (_, comp) = do 
    when dbg $ liftIO $ printf "spawning Accelerate computation\n"
    iv <- new
    let wrappedComp = do
          when dbg $ printf "running Accelerate computation\n"
          ans <- evaluate $ Acc.run comp
          R.pushL resultQueue $ do
            when dbg $ liftIO $ printf "Accelerate computation finished\n"
            put_ iv (toIArray ans)
    liftIO $ writeChan gpuQueue wrappedComp
    return iv

-- | Backstealing spawn where the result is converted to a
-- 'Data.Vector.Unboxed.Vector'. Since the result of either 'Par' or
-- the 'Acc' version may be put in the resulting 'IVar', it is
-- expected that the result of both computations is an equivalent
-- 'Vector'. /TODO/: make a variant with unrestricted 'Shape' that,
-- e.g., yields a vector in row-major order.
spawnAccVector :: (Vector.Unbox a, Elt a)
               => (Par (Vector.Vector a), Acc (Array DIM1 a))
               -> Par (IVar (Vector.Vector a))
spawnAccVector (_, comp) = do 
    when dbg $ liftIO $ printf "spawning Accelerate computation\n"
    iv <- new
    let wrappedComp = do
          when dbg $ printf "running Accelerate computation\n"
          ans <- evaluate $ Acc.run comp
          R.pushL resultQueue $ do
            when dbg $ liftIO $ printf "Accelerate computation finished\n"
            put_ iv (arrToVector ans)
    liftIO $ writeChan gpuQueue wrappedComp
    return iv

arrToVector :: Vector.Unbox a => Array DIM1 a -> Vector.Vector a
arrToVector arr = Vector.fromList (toList arr)

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