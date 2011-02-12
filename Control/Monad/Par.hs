{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

-- | This module provides a deterministic parallelism monad, @Par@.
-- @Par@ is for speeding up pure computations; it cannot be used for
-- IO (for that, see @Control.Concurrent@).  The result of a given
-- @Par@ computation is always the same - ie. it is deterministic, but
-- the computation may be performed more quickly if there are
-- processors available to share the work.
--
-- For example, the following program fragment computes the values of
-- @(f x)@ and @(g x)@ in parallel, and returns a pair of their results:
--
-- >  runPar $ do
-- >      a <- pval (f x)
-- >      b <- pval (g x)
-- >      return (a,b)
--
-- @Par@ can be used for specifying pure parallel computations in
-- which the order of the computation is not known beforehand;
-- that is, the programmer specifies how information flows from one
-- part of the computation to another, but not the order in which
-- computations will be evaluated at runtime.  Information flow is
-- described using "variables" called @PVar@s, which support 'put' and
-- 'get' operations.  For example, the following defines a small
-- network with 4 data values in a diamond-shaped data flow:
--
-- >   runPar $ do
-- >       [a,b,c,d] <- sequence [new,new,new,new]
-- >       fork $ do x <- get a; put b (x+1)
-- >       fork $ do x <- get a; put c (x+2)
-- >       fork $ do x <- get b; y <- get c; put d (x+y)
-- >       fork $ do put a (3 :: Int)
-- >       get d
--
-- The result of this computation is always 9.  The 'get' operation
-- waits until its input is available; multiple 'put's to the same
-- @PVar@ are not allowed, and result in a runtime error.  Values
-- stored in @PVar@s are usually fully evaluated (although there are
-- ways provided to pass lazy values if necessary).
--
-- Unlike @Control.Parallel@, in @Control.Monad.Par@ parallelism is
-- not combined with laziness, so sharing and granulairty are
-- completely under the control of the programmer.  New units of
-- parallel work are only created by @fork@, @par@, and a few other
-- combinators.
--
-- The implementation is based on a work-stealing scheduler that
-- divides the work as evenly as possible betwen the available
-- processors at runtime.
--

module Control.Monad.Par (
    Par,
    runPar,
    fork,
    new,
    get,
    put, put_,
    both,
    pval,
    spawn, spawn_,
    parMap,
  ) where

import Control.Monad
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent
import GHC.Conc hiding (par)
import Control.DeepSeq
-- import Text.Printf

-- ---------------------------------------------------------------------------

data Trace = forall a . Get (PVar a) (a -> Trace)
           | forall a . Put (PVar a) a Trace
           | forall a . New (PVar a -> Trace)
           | Fork Trace Trace
           | Done

sched :: Sched -> Trace -> IO ()
sched queue t = case t of
    New f -> do
      r <- newIORef Empty
      sched queue (f (PVar r))
    Get (PVar v) c -> do
      e <- readIORef v
      case e of
         Full a   -> sched queue (c a)
         _other -> do
           r <- atomicModifyIORef v $ \e -> case e of
                        Empty    -> (Blocked [c], reschedule queue)
                        Full a   -> (Full a,       sched queue (c a))
                        Blocked cs -> (Blocked (c:cs), reschedule queue)
           r
    Put (PVar v) a t  -> do
      cs <- atomicModifyIORef v $ \e -> case e of
               Empty    -> (Full a, [])
               Full _   -> error "multiple put"
               Blocked cs -> (Full a, cs)
      mapM_ (pushWork queue. ($a)) cs
      sched queue t
    Fork child parent -> do
         pushWork queue child
         sched queue parent
    Done ->
         reschedule queue

reschedule :: Sched -> IO ()
reschedule queue@Sched{ workpool } = do
  e <- atomicModifyIORef workpool $ \ts ->
         case ts of
           []      -> ([], Nothing)
           (t:ts') -> (ts', Just t)
  case e of
    Nothing -> steal queue
    Just t  -> sched queue t

steal :: Sched -> IO ()
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
              sched q t
           Nothing -> go xs

pushWork :: Sched -> Trace -> IO ()
pushWork Sched { workpool, idle } t = do
  atomicModifyIORef workpool $ \ts -> (t:ts, ())
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up

data Sched = Sched
    { no       :: {-# UNPACK #-} !Int,
      workpool :: IORef [Trace],
      idle     :: IORef [MVar Bool],
      scheds   :: [Sched]
    }
--  deriving Show

newtype Par a = Par {
    runCont :: (a -> Trace) -> Trace
}

instance Functor Par where
    fmap f m = Par $ \c -> runCont m (c . f)

instance Monad Par where
    return a = Par ($ a)
    m >>= k  = Par $ \c -> runCont m $ \a -> runCont (k a) c

newtype PVar a = PVar (IORef (PVal a))

data PVal a = Full a | Empty | Blocked [a -> Trace]

runPar :: Par a -> a
runPar x = unsafePerformIO $ do
   let n = numCapabilities
   workpools <- replicateM n $ newIORef []
   idle <- newIORef []
   let (main:others) = [ Sched { no=x, workpool=wp, idle, scheds=(main:others) }
                       | (x,wp) <- zip [1..] workpools ]
   r <- newIORef (error "runPar completed prematurely without a result")
   forM_ (zip [2..] others) $ \(cpu,sched) -> forkOnIO cpu $ reschedule sched
   sched main $
     runCont x $ \a -> unsafePerformIO (writeIORef r a >> return Done)
   readIORef r

-- | forks a computation to happen in parallel.  The forked
-- computation may exchange values with other computations using
-- @PVar@s.
fork :: Par () -> Par ()
fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

-- > both a b >> c  ==   both (a >> c) (b >> c)
-- is this useful for anything?
both :: Par a -> Par a -> Par a
both a b = Par $ \c -> Fork (runCont a c) (runCont b c)

-- -----------------------------------------------------------------------------

-- | creates a new @PVar@
new :: Par (PVar a)
new  = Par $ New

-- | read the value in a @PVar@.  The 'get' can only return when the
-- value has been written by a prior or parallel @put@ to the same
-- @PVar@.
get :: PVar a -> Par a
get v = Par $ \c -> Get v c

-- | like 'put', but only head-strict rather than fully-strict.
put_ :: PVar a -> a -> Par ()
put_ v !a = Par $ \c -> Put v a (c ())

-- | put a value into a @PVar@.  Multiple 'put's to the same @PVar@
-- are not allowed, and result in a runtime error.
--
-- 'put' fully evaluates its argument, which therefore must be an
-- instance of 'NFData'.  The idea is that this forces the work to
-- happen when we expect it, rather than being passed to the consumer
-- of the @PVar@ and performed later, which often results in less
-- parallelism than expected.
--
-- Sometimes partial strictness is more appropriate: see 'put_'.
--
put :: NFData a => PVar a -> a -> Par ()
put v a = deepseq a (Par $ \c -> Put v a (c ()))

-- -----------------------------------------------------------------------------

-- | Like 'spawn', but the result is only head-strict, not fully-strict.
spawn_ :: Par a -> Par (PVar a)
spawn_ p = do
  r <- new
  fork (p >>= put_ r)
  return r

-- | Like 'fork', but returns a @PVar@ that can be used to query the
-- result of the forked computataion.
spawn :: NFData a => Par a -> Par (PVar a)
spawn p = do
  r <- new
  fork (p >>= put r)
  return r

-- | equivalent to @forkR . return@
pval :: NFData a => a -> Par (PVar a)
pval a = spawn (return a)

parMap :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMap f xs = mapM (spawn . f) xs >>= mapM get

-- -----------------------------------------------------------------------------

test :: IO ()
test = do
  print ((runPar $ return 3) :: Int)
  print (runPar $ do r <- new; put r (3 :: Int); get r)
  print (runPar $ do r <- new; fork (put r (3::Int)); get r)
  print ((runPar $ do r <- new; get r)  :: Int)

test2 :: Int
test2 =  runPar $ do
      [a,b,c,d] <- sequence [new,new,new,new]
      fork $ do x <- get a; put b (x+1)
      fork $ do x <- get a; put c (x+2)
      fork $ do x <- get b; y <- get c; put d (x+y)
      fork $ do put a 3
      get d

test3 :: Int
test3 = runPar $ do
   a <- new
   put a (3::Int)
   both (return 1) (return 2)
