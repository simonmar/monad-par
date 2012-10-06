{-# LANGUAGE PackageImports, CPP, 
    GeneralizedNewtypeDeriving 
 #-}

-- | Type definiiton and some helpers.  This is used mainly by
-- Direct.hs but can also be used by other modules that want access to
-- the internals of the scheduler (i.e. the private type constructor).

module Control.Monad.Par.Scheds.DirectInternal where

import Control.Applicative
import "mtl" Control.Monad.Cont as C
import qualified "mtl" Control.Monad.Reader as RD

import qualified System.Random.MWC as Random

import Control.Concurrent hiding (yield)
import GHC.Conc
import Data.IORef
import Data.Concurrent.Deque.Class (WSDeque)
-- import Data.Concurrent.Deque.Reference.DequeInstance
-- import Data.Concurrent.Deque.Reference as R
import Data.Concurrent.Deque.Class (WSDeque)
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Concurrent.Deque.Reference as R


-- Our monad stack looks like this:
--      ---------
--        ContT
--       ReaderT
--         IO
--      ---------
-- The ReaderT monad is there for retrieving the scheduler given the
-- fact that the API calls do not get it as an argument.
-- 
-- Note that the result type for continuations is unit.  Forked
-- computations return nothing.
--
newtype Par a = Par { unPar :: C.ContT () ROnly a }
    deriving (Monad, MonadCont, RD.MonadReader Sched)
type ROnly = RD.ReaderT Sched IO


data Sched = Sched 
    { 
      ---- Per worker ----
      no       :: {-# UNPACK #-} !Int,
#ifdef REACTOR_DEQUE
      workpool :: R.Deque IOArray (Par ()),
#else
      workpool :: WSDeque (Par ()),
#endif
      rng      :: HotVar Random.GenIO, -- Random number gen for work stealing.
      isMain :: Bool, -- Are we the main/master thread? 

      ---- Global data: ----
      killflag :: HotVar Bool,
      idle     :: HotVar [MVar Bool],
      scheds   :: [Sched],   -- A global list of schedulers.

      -- For nested support, our scheduler may be working on behalf of
      -- a REAL haskell continuation that we need to return to.  In
      -- that case we need to know WHEN to stop rescheduling and
      -- return to that genuine continuation.
      sessionFinished :: Maybe (HotVar Bool)
     }


--------------------------------------------------------------------------------
-- Helpers #1:  Atomic Variables
--------------------------------------------------------------------------------
-- TEMP: Experimental

#ifndef HOTVAR
#define HOTVAR 1
#endif
newHotVar      :: a -> IO (HotVar a)
modifyHotVar   :: HotVar a -> (a -> (a,b)) -> IO b
modifyHotVar_  :: HotVar a -> (a -> a) -> IO ()
writeHotVar    :: HotVar a -> a -> IO ()
readHotVar     :: HotVar a -> IO a
-- readHotVarRaw  :: HotVar a -> m a
-- writeHotVarRaw :: HotVar a -> m a

{-# INLINE newHotVar     #-}
{-# INLINE modifyHotVar  #-}
{-# INLINE modifyHotVar_ #-}
{-# INLINE readHotVar    #-}
{-# INLINE writeHotVar   #-}


#if HOTVAR == 1
type HotVar a = IORef a
newHotVar     = newIORef
modifyHotVar  = atomicModifyIORef
modifyHotVar_ v fn = atomicModifyIORef v (\a -> (fn a, ()))
readHotVar    = readIORef
writeHotVar   = writeIORef
instance Show (IORef a) where 
  show ref = "<ioref>"

-- hotVarTransaction = id
hotVarTransaction = error "Transactions not currently possible for IO refs"
readHotVarRaw  = readHotVar
writeHotVarRaw = writeHotVar


#elif HOTVAR == 2 
#warning "Using MVars for hot atomic variables."
-- This uses MVars that are always full with *something*
type HotVar a = MVar a
newHotVar   x = do v <- newMVar; putMVar v x; return v
modifyHotVar  v fn = modifyMVar  v (return . fn)
modifyHotVar_ v fn = modifyMVar_ v (return . fn)
readHotVar    = readMVar
writeHotVar v x = do swapMVar v x; return ()
instance Show (MVar a) where 
  show ref = "<mvar>"

-- hotVarTransaction = id
-- We could in theory do this by taking the mvar to grab the lock.
-- But we'd need some temporary storage....
hotVarTransaction = error "Transactions not currently possible for MVars"
readHotVarRaw  = readHotVar
writeHotVarRaw = writeHotVar


#elif HOTVAR == 3
#warning "Using TVars for hot atomic variables."
-- Simon Marlow said he saw better scaling with TVars (surprise to me):
type HotVar a = TVar a
newHotVar = newTVarIO
modifyHotVar  tv fn = atomically (do x <- readTVar tv 
				     let (x2,b) = fn x
				     writeTVar tv x2
				     return b)
modifyHotVar_ tv fn = atomically (do x <- readTVar tv; writeTVar tv (fn x))
readHotVar x = atomically $ readTVar x
writeHotVar v x = atomically $ writeTVar v x
instance Show (TVar a) where 
  show ref = "<tvar>"

hotVarTransaction = atomically
readHotVarRaw  = readTVar
writeHotVarRaw = writeTVar

#endif
