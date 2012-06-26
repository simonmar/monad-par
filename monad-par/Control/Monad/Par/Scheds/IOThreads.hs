{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

{- LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP, ScopedTypeVariables,
             TypeSynonymInstances, ,
             GeneralizedNewtypeDeriving, RecordWildCards
	     -}

-- | This is a degenerate scheduler in which fork corresponds directly
--   to forkIO.

module Control.Monad.Par.Scheds.IOThreads (
    Par, IVar(..), 
    runPar, 
    new, get, put_, fork,
    newFull, newFull_, put,
    spawn, spawn_, spawnP
--                   spawn1_, 
 ) where

import Control.Applicative (Applicative, (<$>))
import Control.Concurrent hiding (yield)
import qualified Control.Monad.Par.Class as PC
import Control.DeepSeq (NFData, deepseq)
import Data.IORef
import GHC.IO (unsafePerformIO, unsafeDupablePerformIO)

--------------------------------------------------------------------------------
-- This implementation is naturally very short:

newtype Par a = Par { unPar :: IO a }
 deriving (Monad, Functor, Applicative)

newtype IVar a = IVar (IORef (IVarContents a))
data IVarContents a = Full a | Empty | Blocked (MVar a) 

fork (Par task) = Par $ do forkIO task; return ()

get (IVar ref) = Par $ do   
  -- Optimistically, let's assume it's either full or we're not the first one to block:
  x <- readIORef ref
  case x of 
    Full v     -> return v
    Blocked mv -> readMVar mv
    Empty      -> do  
      -- Ok, no luck, we need to allocate an MVar, which might be wasted:
      mv <- newEmptyMVar
      b <- atomicModifyIORef ref $ \ new ->
            case new of 
              Empty       -> (Blocked mv, Right mv)
              -- Otherwise someone raced in and changed it!:
              Blocked mv2 -> (new,        Right mv2)
              Full a      -> (new,        Left  a )
      case b of 
        Left a   -> return a
        Right mv -> readMVar mv

put_ (IVar ref) val = Par $ do
  mmv <- atomicModifyIORef ref $ \ old -> 
    case old of 
      Empty      -> (Full val, Nothing)
      Full a     -> error "Multiple writes to IVar!"
      Blocked mv -> (Full val, Just mv)
  case mmv of   
    Nothing -> return ()
    Just mv -> putMVar mv val

new          = Par $ IVar <$> newIORef Empty
newFull_ val = Par $ IVar <$> newIORef (Full val)

runPar = unsafePerformIO . unPar

--------------------------------------------------------------------------------
-- Boilerplate and instances shared across schedulers:
--------------------------------------------------------------------------------

put iv val = deepseq val $ put_ iv val

newFull :: NFData a => a -> Par (IVar a)
newFull a = deepseq a (newFull a)

-- Here we use the same mechanism an in Spark.hs:
spawnP :: NFData a => a -> Par (IVar a)
spawnP val = do mv <- new 
                fork (put mv val)                
                return mv

spawn p  = do r <- new;  fork (p >>= put r);   return r
spawn_ p = do r <- new;  fork (p >>= put_ r);  return r


fork   :: Par () -> Par ()
spawn  :: NFData a => Par a -> Par (IVar a)
spawn_ :: Par a -> Par (IVar a)
put_   :: IVar a -> a -> Par ()
put    :: NFData a => IVar a -> a -> Par ()
get    :: IVar a -> Par a
runPar :: Par a -> a 
newFull_ ::  a -> Par (IVar a)

instance PC.ParFuture IVar Par where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar IVar Par where
  fork = fork
  new  = new
  put_ = put_
  newFull = newFull
  newFull_ = newFull_
