{-# LANGUAGE PackageImports, CPP #-}

-- | Type definiiton, shared by multiple modules.

module Control.Monad.Par.Scheds.DirectInternal where

import Control.Applicative
import "mtl" Control.Monad.Cont as C
import qualified "mtl" Control.Monad.Reader as RD

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
      rng      :: HotVar GenIO, -- Random number gen for work stealing.
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
