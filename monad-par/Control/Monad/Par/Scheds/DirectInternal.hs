{-# LANGUAGE PackageImports, CPP, GeneralizedNewtypeDeriving,
             DeriveDataTypeable #-}

-- | Type definition and some helpers.  This is used mainly by
-- Direct.hs but can also be used by other modules that want access to
-- the internals of the scheduler (i.e. the private `Par` type constructor).

module Control.Monad.Par.Scheds.DirectInternal where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif

import Control.Applicative
import "mtl" Control.Monad.Cont as C
import qualified "mtl" Control.Monad.Reader as RD
import "mtl" Control.Monad.Trans (liftIO)

import qualified System.Random.MWC as Random

import Control.Concurrent hiding (yield)
import GHC.Conc
import Data.IORef
import qualified Data.Set as S
import Data.Word (Word64)
import Data.Concurrent.Deque.Class (WSDeque)
import Control.Monad.Fix (MonadFix (mfix))
#if MIN_VERSION_base(4,9,0)
import GHC.IO.Unsafe (unsafeDupableInterleaveIO)
#else
import System.IO.Unsafe (unsafeInterleaveIO)
#endif

#ifdef USE_CHASELEV
#warning "Note: using Chase-Lev lockfree workstealing deques..."
import Data.Concurrent.Deque.ChaseLev.DequeInstance
import Data.Concurrent.Deque.ChaseLev as R
#endif

import Data.Typeable (Typeable)
import Control.Exception (Exception, throwIO, BlockedIndefinitelyOnMVar (..),
                          catch)

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
    deriving (Functor, Applicative, Monad, MonadCont, RD.MonadReader Sched)
type ROnly = RD.ReaderT Sched IO

instance MonadFix Par where
  mfix = fixPar

-- | Take the monadic fixpoint of a 'Par' computation. This is
-- the definition of 'mfix' for 'Par'. Throws 'FixParException'
-- if the result is demanded strictly within the computation.
fixPar :: (a -> Par a) -> Par a
-- We do this IO-style, rather than ST-style, in order to get a
-- consistent exception type. Using the ST-style mfix, a strict
-- argument could lead us to *either* a <<loop>> exception *or*
-- (if the wrong sort of computation gets re-run) a "multiple-put"
-- error.
fixPar f = Par $ ContT $ \ar -> RD.ReaderT $ \sched -> do
  mv <- newEmptyMVar
  ans <- unsafeDupableInterleaveIO (readMVar mv `catch`
      \ ~BlockedIndefinitelyOnMVar -> throwIO FixParException)
  flip RD.runReaderT sched $
    runContT (unPar (f ans)) $ \a -> liftIO (putMVar mv a) >> ar a

#if !MIN_VERSION_base(4,9,0)
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO = unsafeInterleaveIO
#endif

data FixParException = FixParException deriving (Show, Typeable)
instance Exception FixParException

type SessionID = Word64

-- An ID along with a flag to signal completion:
data Session = Session SessionID (HotVar Bool)

data Sched = Sched
    {
      ---- Per worker ----
      no       :: {-# UNPACK #-} !Int,
      workpool :: WSDeque (Par ()),
      rng      :: HotVar Random.GenIO, -- Random number gen for work stealing.
      isMain :: Bool, -- Are we the main/master thread?

      -- The stack of nested sessions that THIS worker is participating in.
      -- When a session finishes, the worker can return to its Haskell
      -- calling context (it's "real" continuation).
      sessions :: HotVar [Session],
      -- (1) This is always non-empty, containing at least the root
      --     session corresponding to the anonymous system workers.
      -- (2) The original invocation of runPar also counts as a session
      --     and pushes a second
      -- (3) Nested runPar invocations may push further sessions onto the stack.

      ---- Global data: ----
      idle     :: HotVar [MVar Bool], -- waiting idle workers
      scheds   :: [Sched],            -- A global list of schedulers.

      -- Any thread that enters runPar (original or nested) registers
      -- itself in this global list.  When the list becomes null,
      -- worker threads may shut down or at least go idle.
      activeSessions :: HotVar (S.Set SessionID),

      -- A counter to support unique session IDs:
      sessionCounter :: HotVar SessionID
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
  show _ref = "<ioref>"

writeHotVarRaw :: HotVar a -> a -> IO ()
-- hotVarTransaction = id
hotVarTransaction = error "Transactions not currently possible for IO refs"
readHotVarRaw :: HotVar a -> IO a
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
  show _ref = "<mvar>"

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
