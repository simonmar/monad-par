{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns, ScopedTypeVariables,
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
    parMap, parMapM,
  ) where

import Data.Traversable
import Control.Monad hiding (mapM, sequence, join)
import Prelude hiding (mapM, sequence, head,tail)
import Data.IORef
import System.IO.Unsafe
import GHC.IO (unsafeDupablePerformIO)
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

   rref <- newIORef Empty
   forM_ (zip [2..] others) $ \(cpu,sched) -> forkOnIO cpu $ reschedule sched
   sched main $ runCont (x >>= put_ (PVar rref)) (const Done)
   r <- readIORef rref
   case r of
     Full a -> return a
     _ -> error "no result"

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
-- Derived functions

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

-- -----------------------------------------------------------------------------
-- Parallel maps over Traversable data structures

parMap :: (Traversable t, NFData b) => (a -> b) -> t a -> Par (t b)
parMap f xs = mapM (pval . f) xs >>= mapM get

parMapM :: (Traversable t, NFData b) => (a -> Par b) -> t a -> Par (t b)
parMapM f xs = mapM (spawn . f) xs >>= mapM get

{-# SPECIALISE parMap  :: (NFData b) => (a -> b)     -> [a] -> Par [b] #-}
{-# SPECIALISE parMapM :: (NFData b) => (a -> Par b) -> [a] -> Par [b] #-}


-- | parMapReduceRange is similar to the "parallel for" construct
--   found in many parallel programming models.  A range 
-- 
--parFoldRange :: Int -> (Int -> acc -> Par acc) -> acc -> Int -> Int -> Par acc
parMapReduceRange :: Int -> Int -> Int -> (Int -> Par a) -> (a -> a -> a) -> a -> Par a
parMapReduceRange threshold min max fn binop init = loop min max 
 where 
  loop min max 
    | max - min <= threshold = 
	let mapred a b = do x <- fn b; 
			    x `seq` return (a `binop` x) 
	in foldM mapred init [min..max]

    | otherwise  = do
	let mid = min + ((max - min) `quot` 2)
	rght <- spawn_ $ loop (mid+1) max 
	left <- spawn_ $ loop  min    mid 
	r <- get rght
	l <- get left
	return (l `binop` r)

-- A version that works for any splittable input domain.  In this case
-- the "threshold" is a predicate on inputs.
-- parMapReduceRangeGeneric :: (inp -> Bool) -> (inp -> Maybe (inp,inp)) -> inp -> 

-- -----------------------------------------------------------------------------
-- Open Lists -- PVars at the tail.
-- TODO: FACTOR INTO SEPARATE FILE.

-- These have some of the advantages of imperative lists, such as
-- constant time appending, while retaining determinism and having
-- O(1) access to the head of the list unlike tree-based lists (e.g. append
-- rather than cons-based).

data IList a = Null | Cons { hd :: a, tl :: PVar (IList a) }

data OpenList a = OpenList (IList a) (IList a)

-- | To fully evaluate an open list means to evaluate all of the
--   available car fields.  There is nothing to be done about the fact
--   that the trailing PVar cdr field may receive further extensions.
instance NFData a => NFData (IList a) where 
--  rnf Null = r0
  rnf Null = ()
  rnf (Cons a b) = rnf a `seq` rnf b

-- | Forcing evaluation of a PVar is fruitless.
instance NFData (PVar a) where
  rnf _ = ()

-- | An empty open list.
empty :: OpenList a
empty = OpenList Null Null

-- | A single element open list.
singleton :: a -> Par (OpenList a)
singleton x = 
  do pv <- new 
     let cell = Cons x pv
     return (OpenList cell cell)

-- | Terminate an open list so that it cannot be extended further.
close :: NFData a => OpenList a -> Par ()
close (OpenList Null _) = return ()
close (OpenList _   tp) = put (tl tp) Null

-- | Destructive append operation.
join :: NFData a => OpenList a -> OpenList a -> Par (OpenList a)
join (OpenList Null _) right = return right
join left  (OpenList Null _) = return left 
join (OpenList hp1 tp1) (OpenList hp2 tp2) =
    do put (tl tp1) hp2
       return (OpenList hp1 tp2)

head :: OpenList a -> a
head (OpenList Null _) = error "cannot take head of null OpenList"
head (OpenList hp _)   = hd hp

tail :: OpenList a -> Par (OpenList a)
tail (OpenList Null _) = error "cannot take tail of null OpenList"
tail (OpenList hp tp)  = 
  do nxt <- get (tl hp)
     return (OpenList nxt tp)

-- Not sure yet if we want this for PVars:
--newFilled :: NFData a => a -> Par (PVar a)
newFilled :: a -> Par (PVar a)
newFilled x = 
  do let ref = unsafeDupablePerformIO$ newIORef (Full x)
     return (PVar ref)
-- Here's the reference implementation:
-- newFilled x = 
--   do v <- new 
--      put_ v x
--      return v

cons :: NFData a => a -> OpenList a -> Par (OpenList a)
cons car (OpenList hp tp) = 
  do cdr <- newFilled hp
     return (OpenList (Cons car cdr) tp)

newCell x = do pv <-new; return (Cons x pv)

-- | Convert a list to an OpenList, open to extension at the tail.
fromList :: NFData a => [a] -> Par (OpenList a)
fromList [] = return empty
fromList ls@(h:t) = 
  --   This function is inefficient and could be replaced with an additional IList data constructor.
    do head <- newCell h
       rest <- loop head t
       return (OpenList head rest)
 where 
   loop last  []   = return last
   loop last (h:t) = 
    do cell <- newCell h
       put (tl last) cell
       loop cell t
       
-- | Convert a CLOSED OpenList to a list. 
toList :: NFData a => (OpenList a) -> Par [a] 
-- Note: presently not tail-recursive:
toList (OpenList hp _) = loop hp
 where 
  loop Null = return []
  loop (Cons head pv) = 
    do 
       rest <- get pv
       converted <- loop rest
       return (head : converted)
    

-- | OpenLists can only be printed properly in the Par monad.
instance Show a => Show (OpenList a) where 
  show (OpenList Null _) = "OpenList []"
  show (OpenList (Cons fst _) (Cons lst _)) = 
      "OpenList ["++show fst++".."++ show lst ++"]"
       



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

test_pmrr1 :: Int
test_pmrr1 = runPar$ parMapReduceRange 1 1 100 (return) (+) 0


test_ol1 :: OpenList Int
test_ol1 = runPar$ empty `join` empty

test_ol2 :: String
test_ol2 = show$ runPar$ do 
 ls1 <- fromList [10,11,12]
 ls2 <- singleton (5::Int)
 join ls1 ls2


-- 1..10
test_ol3 :: [Int]
test_ol3 = runPar$ do ol :: OpenList Int <- fromList [1..10]
		      close ol
		      toList ol
-- 3
test_ol4 :: Int
test_ol4 = runPar$ do ol <- fromList [1..10]		      
		      t1 <- tail ol
		      t2 <- tail t1
		      return (head t2)


-- TODO: HUnit These.
