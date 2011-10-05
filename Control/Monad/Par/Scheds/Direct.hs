{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification, CPP
	     #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

-- A scheduler for the Par monad based on directly performing IO actions.


-- | This module exposes the internals of the @Par@ monad so that you
-- can build your own scheduler or other extensions.  Do not use this
-- module for purposes other than extending the @Par@ monad with new
-- functionality.

module Control.Monad.ParDirect (
   Sched(..), Par(..),
   IVar(..), IVarContents(..),
--    sched,
--    runPar, 
    new, get, put_
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

-- import GHC.Conc
import Control.Monad.Cont as C
import qualified Control.Monad.Reader as R
-- import qualified Data.Array as A
import qualified Data.Vector as A
import qualified Data.Sequence as Seq
import System.Random as Random
import System.IO.Unsafe (unsafePerformIO)

{-# INLINE get  #-}
{-# INLINE put_ #-}
{-# INLINE new  #-}

-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------

-- Our monad stack looks like this:
--      ---------
--        ContT
--       ReaderT
--         IO
--      ---------
-- The ReaderT monad is there for retrieving the scheduler given the
-- fact that the API calls do not get it as an argument.
type Par a = C.ContT () (R.ReaderT Sched IO) a

data Sched = Sched 
    { 
      no       :: {-# UNPACK #-} !Int,
      workpool :: IORef (Deque (Par ())),

--      workpool :: A.Vector (IORef (Deque (Par ()))),
--      randoms  :: A.Vector (IORef Random.StdGen),
      myid     :: Int,
      killflag :: IORef Bool,

      idle     :: IORef [MVar Bool],

      scheds   :: [Sched] -- A global list of schedulers.
     }

newtype IVar a = IVar (IORef (IVarContents a))

data IVarContents a = Full a | Empty | Blocked [a -> Par ()]

-- instance NFData (IVar a) where
--   rnf _ = ()

runPar :: Par a -> a 
runPar x = unsafePerformIO $ do
  state <- defaultState
  r <- newIORef (error "this should not happen")
  R.runReaderT (C.runContT x (C.liftIO . writeIORef r)) state
  readIORef r

defaultState = undefined


liIO = C.liftIO
liR  = id
liC  = id

-- type M r a = C.ContT r (R.ReaderT Sched IO) a
-- mycallCC :: ((forall b . a -> M r b) -> M r a) -> M r a
-- mycallCC f = C.ContT $ \c -> C.runContT (f (\a -> C.ContT $ \_ -> c a)) c

type M a = C.ContT () (R.ReaderT Sched IO) a
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
get (IVar v) =  mycallCC $ \cont -> 
   do
      e <- liIO$ readIORef v
      case e of
         Full a -> return a
         _ -> liIO$ do
           r <- atomicModifyIORef v $ \e -> case e of
                     Empty      -> (Blocked [cont6], scheduler)
                     Full a     -> (Full a, return a)
                     Blocked cs -> (Blocked (cont:cs), scheduler)
           r

scheduler = undefined

#if 0
reschedule :: StepCode a
reschedule = C.ContT rescheduleR

rescheduleR :: a -> R.ReaderT Sched IO ()
rescheduleR _ = do
  m <- popWork 
  case m of
    Nothing -> return ()
    Just (C.ContT f)  -> f rescheduleR
#endif

-- -- | like 'put', but only head-strict rather than fully-strict.
put_ :: IVar a -> a -> Par ()
put_ (IVar v) !a = do
   sched <- C.lift R.ask 
   liIO$ do 
      cs <- atomicModifyIORef v $ \e -> case e of
               Empty    -> (Full a, [])
               Full _   -> error "multiple put"
               Blocked cs -> (Full a, cs)
      -- TODO spawn woken work on FRONT of queue..
--      mapM_ (pushWork sched . ($a)) cs
      return ()


-- | If any worker is idle, wake one up and give it work to do.
pushWork :: Sched -> Par () -> IO ()
pushWork Sched { workpool, idle } t = do
  atomicModifyIORef_ workpool (addfront t)
#if 0
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up
#endif


fork = undefined
--     Fork child parent -> do
--          pushWork queue child
--          loop parent


{-# INLINE atomicModifyIORef_ #-}
atomicModifyIORef_ ref f = atomicModifyIORef ref (\x -> (f x, ()))
