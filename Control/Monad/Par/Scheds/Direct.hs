{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- A scheduler for the Par monad based on directly performing IO
-- actions when Par methods are called (i.e. without using a lazy
-- trace data structure).

module Control.Monad.Par.Scheds.Direct (
   Sched(..), Par(..),
   IVar(..), IVarContents(..),
--    sched,
    runPar, 
    new, get, put_, fork
    -- newFull, newFull_, put,

--   runParAsync, runParAsyncHelper,
--   pollIVar, yield,
 ) where


-- import Control.Monad as M hiding (mapM, sequence, join)
-- import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
-- import System.IO.Unsafe
import Control.Concurrent hiding (yield)
-- import GHC.Conc hiding (yield)
-- import Control.DeepSeq
-- import Control.Applicative
-- import Text.Printf

import GHC.Conc
import Control.Monad.Cont as C
import qualified Control.Monad.Reader as R
-- import qualified Data.Array as A
import qualified Data.Vector as A
import qualified Data.Sequence as Seq
import System.Random as Random
import System.IO.Unsafe (unsafePerformIO)

import qualified Control.Monad.ParClass as PC

--------------------------------------------------------------------------------
-- Core type definitions
--------------------------------------------------------------------------------

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
-- type Par a = C.ContT () (R.ReaderT Sched IO) a
type Par a = R.ReaderT Sched ContIO a
-- type ContIO = C.ContT () IO
type ContIO = C.ContT () IO


data Sched = Sched 
    { 
      no       :: {-# UNPACK #-} !Int,
      workpool :: HotVar (Deque (Par ())),

--      workpool :: A.Vector (IORef (Deque (Par ()))),
--      randoms  :: A.Vector (IORef Random.StdGen),
      myid     :: Int,
      killflag :: HotVar Bool,

      idle     :: HotVar [MVar Bool],

      scheds   :: [Sched] -- A global list of schedulers.
     }

newtype IVar a = IVar (IORef (IVarContents a))

data IVarContents a = Full a | Empty | Blocked [a -> ContIO ()]


--------------------------------------------------------------------------------
-- Helpers #1:  Simple Deques
--------------------------------------------------------------------------------

--atomicModifyIORef :: IORef a -> (a -> (a,b)) -> IO b
--atomicModifyIORef (IORef (STRef r#)) f = IO $ \s -> atomicModifyMutVar# r# f s

casIORef = undefined

emptydeque :: Deque a 
addfront  :: a -> Deque a -> Deque a
addback   :: a -> Deque a -> Deque a

-- takefront :: Deque a -> Maybe (Deque a, a)
takefront :: Deque a -> (Deque a, Maybe a)
takeback  :: Deque a -> (Deque a, Maybe a)

-- [2011.03.21] Presently lists are out-performing Seqs:
#if 1
newtype Deque a = DQ [a]
emptydeque = DQ []

addfront x (DQ l)    = DQ (x:l)
addback x (DQ [])    = DQ [x]
addback x (DQ (h:t)) = DQ (h : rest)
 where DQ rest = addback x (DQ t)

takefront (DQ [])     = (emptydeque, Nothing)
takefront (DQ (x:xs)) = (DQ xs, Just x)
takeback  (DQ [])     = (emptydeque, Nothing)
takeback  (DQ ls)     = (DQ rest, Just final)
 where 
  (final,rest) = loop ls []
  loop [x]    acc = (x, reverse acc)
  loop (h:tl) acc = loop tl (h:acc)
#endif

--------------------------------------------------------------------------------
-- Helpers #2:  Atomic Variables
--------------------------------------------------------------------------------
-- TEMP: Experimental

#ifndef HOTVAR
#define HOTVAR 3
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


-----------------------------------------------------------------------------
-- Helpers #3:  Pushing and popping work.
-----------------------------------------------------------------------------

{-# INLINE pushWork #-}
{-# INLINE popWork  #-}

-- | If any worker is idle, wake one up and give it work to do.
-- pushWork :: Sched -> ContIO () -> IO ()
-- popWork  :: Sched -> IO (Maybe (ContIO ()))

-- pushWork :: Sched -> (Sched -> ContIO ()) -> IO ()
-- pushWork Sched { workpool } fn = undefined

pushWork :: Sched -> Par () -> IO ()
popWork  :: Sched -> IO (Maybe (Par ()))

popWork  Sched{ workpool }    = modifyHotVar  workpool  takefront

-- Simple pushWork
pushWork Sched { workpool } t = modifyHotVar_ workpool (addfront t)

-- Better pushWork:
-- | Add to the local work-queue.  If any worker is idle, wake one up
--   and give it work to do.
-- pushWork Sched { workpool, idle } t = do
--   modifyHotVar_ workpool (addfront t)
--   idles <- readHotVar idle
--   when (not (null idles)) $ do
--     r <- modifyHotVar idle (\is -> case is of
--                              [] -> ([], return ())
--                              (i:is) -> (is, putMVar i False))
--     r -- wake one up


-- Is there some way to get wakeup off of the spawn path and onto the steal path?
     


--------------------------------------------------------------------------------
-- Running computations in the Par monad
--------------------------------------------------------------------------------

-- instance NFData (IVar a) where
--   rnf _ = ()

runPar :: Par a -> a 
runPar x = unsafePerformIO $ do
  state <- defaultState
  r <- newIORef (error "this should not happen")
  let y = R.runReaderT x state
  C.runContT y (C.liftIO . writeIORef r)
  readIORef r

defaultState = undefined

--------------------------------------------------------------------------------
-- IVar operations
--------------------------------------------------------------------------------

{-# INLINE get  #-}
{-# INLINE put_ #-}
{-# INLINE new  #-}

-- type M a = C.ContT () (R.ReaderT Sched IO) a
-- type M a = Par a
-- mycallCC :: ((forall b . a -> M r b) -> M r a) -> M r a
-- mycallCC f = C.ContT $ \c -> C.runContT (f (\a -> C.ContT $ \_ -> c a)) c

-- Option 2:
type M = ContIO
mycallCC :: ((forall b . a -> M b) -> M a) -> M a
mycallCC f = C.ContT $ \c -> C.runContT (f (\a -> C.ContT $ \_ -> c a)) c

-- | creates a new @IVar@
new :: Par (IVar a)
new  = liftIO $ do r <- newIORef Empty
                   return (IVar r)

-- | read the value in a @IVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @IVar@.
get :: IVar a -> Par a
get (IVar v) =  do 
  sc <- R.ask 
  lift $ 
    mycallCC $ \cont -> 
    do
       e  <- liftIO$ readIORef v
 --      sc <- liR   R.ask
       case e of
	  Full a -> return a
	  _ -> do
--	    let resched = liftIO$ reschedule sc
	    let resched = reschedule sc
            -- TODO: Try NOT using monads as first class values here.  Check for performance effect:
	    r <- liftIO$ atomicModifyIORef v $ \e -> case e of
		      Empty      -> (Blocked [cont], resched)
		      Full a     -> (Full a, return a)
		      Blocked cs -> (Blocked (cont:cs), resched)
	    r

-- | @put_@ is a version of @put@ that is head-strict rather than fully-strict.
put_ :: IVar a -> a -> Par ()
put_ (IVar v) !a = do
--   sched <- C.lift R.ask 
   sched <- R.ask 
   liftIO$ do 
      cs <- atomicModifyIORef v $ \e -> case e of
               Empty    -> (Full a, [])
               Full _   -> error "multiple put"
               Blocked cs -> (Full a, cs)
      -- TODO spawn woken work on FRONT of queue..
      mapM_ (pushWork sched . lift . ($a)) cs
      return ()

-- TODO: Continuation (parent) stealing version.
fork :: Par () -> Par ()
fork task = do
   sch <- R.ask
   liftIO$ pushWork sch task
   

--reschedule :: Sched -> C.ContT () IO a
reschedule :: Sched -> ContIO a 
reschedule mysched = C.ContT rescheduleR
 where 
    rescheduleR :: a -> IO ()
    -- As a continuation, reschedule ignores the value passed to it.
    rescheduleR _ = do
      m <- popWork mysched
      case m of
	Nothing -> return ()
	Just reader  -> 
	   -- Import the work to this thread and tell it what scheduler to use:
	   let C.ContT f = R.runReaderT reader mysched
	   in f rescheduleR 


-- | Attempt to steal work or, failing that, give up and go idle.
steal :: Sched -> IO a
steal = undefined
#if 0
steal q@Sched{ idle, scheds, no=my_no } = do
  -- printf "cpu %d stealing\n" my_no
  go scheds
  where
    go [] = do m <- newEmptyMVar
               r <- atomicModifyIORef idle $ \is -> (m:is, is)
               if length r == numCapabilities - 1
                  then do
                     -- printf "cpu %d initiating shutdown\n" my_no
                     mapM_ (\m -> putMVar m True) r
                  else do
                    done <- takeMVar m
                    if done
                       then do
                         -- printf "cpu %d shutting down\n" my_no
                         return ()
                       else do
                         -- printf "cpu %d woken up\n" my_no
                         go scheds
    go (x:xs)
      | no x == my_no = go xs
      | otherwise     = do
         r <- atomicModifyIORef (workpool x) $ \ ts ->
                 case ts of
                    []     -> ([], Nothing)
                    (x:xs) -> (xs, Just x)
         case r of
           Just t  -> do
              -- printf "cpu %d got work from cpu %d\n" my_no (no x)
              sched True q t
           Nothing -> go xs
#endif




{-# INLINE atomicModifyIORef_ #-}
atomicModifyIORef_ ref f = atomicModifyIORef ref (\x -> (f x, ()))
