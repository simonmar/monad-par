{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns,
             ExistentialQuantification #-}

module ParCont (
    P,
    runP, -- :: P a -> a
    new,  -- :: P (Var a)
    get,  -- :: Var a -> P a
    put,  -- :: Var a -> a -> P a
    fork, -- :: P a -> P a
    forkR,
  ) where

import Control.Monad.Reader as R
import Control.Monad
import Data.IORef
import System.IO.Unsafe
import Control.Concurrent
import GHC.Conc
import Text.Printf

-- ---------------------------------------------------------------------------

data Trace = Yield Trace
           | forall a . Get (PVar a) (a -> Trace)
           | forall a . Put (PVar a) a Trace
           | forall a . New (PVar a -> Trace)
           | Fork Trace Trace
           | Done

sched :: Sched -> Trace -> IO ()
sched queue t = case t of
    Yield t'   -> sched queue t'
    New f -> do
      r <- newIORef (Right [])
      sched queue (f (PVar r))
    Get (PVar v) c -> do
      e <- readIORef v
      case e of
         Left a   -> sched queue (c a)
         Right cs -> do
           r <- atomicModifyIORef v $ \e -> case e of
                        Left a   -> (Left a, sched queue (c a))
                        Right cs -> (Right (c:cs), reschedule queue)
           r
    Put (PVar v) a t  -> do
      cs <- atomicModifyIORef v $ \e -> case e of
               Left a   -> error "multiple put"
               Right cs -> (Left a, cs)
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
pushWork queue@Sched { workpool, idle } t = do
  atomicModifyIORef workpool $ \ts -> (t:ts, ts)
  idles <- readIORef idle
  when (not (null idles)) $ do
    r <- atomicModifyIORef idle (\is -> case is of
                                          [] -> ([], return ())
                                          (i:is) -> (is, putMVar i False))
    r -- wake one up

data Sched = Sched
    { no       :: Int,
      workpool :: IORef [Trace],
      idle     :: IORef [MVar Bool],
      scheds   :: [Sched]
    }
--  deriving Show

-- type SchedR = R.ReaderT Sched IO

newtype P a = P {
    runCont :: (a -> Trace) -> Trace
}

instance Functor P where
    fmap f m = P $ \c -> runCont m (c . f)

instance Monad P where
    return a = P ($ a)
    m >>= k  = P $ \c -> runCont m $ \a -> runCont (k a) c

newtype PVar a = PVar (IORef (Either a [a -> Trace]))

new :: P (PVar a)
new  = P $ New

runP :: P a -> a
runP x = unsafePerformIO $ do
   let n = numCapabilities
   workpools <- replicateM n $ newIORef []
   idle <- newIORef []
   let (main:others) = [ Sched { no=x, workpool=wp, idle, scheds=(main:others) }
                       | (x,wp) <- zip [1..] workpools ]
   r <- newIORef (error "runP completed prematurely without a result")
   forM_ (zip [2..] others) $ \(cpu,sched) -> forkOnIO cpu $ reschedule sched
   sched main $
     runCont x $ \a -> unsafePerformIO (writeIORef r a >> return Done)
   readIORef r

fork :: P () -> P ()
fork p = P $ \c -> Fork (runCont p (\_ -> Done)) (c ())

-- -----------------------------------------------------------------------------


get :: PVar a -> P a
get v = P $ \c -> Get v c

put :: PVar a -> a -> P ()
put v a = P $ \c -> Put v a (c ())


-- -----------------------------------------------------------------------------

forkR :: P a -> P (PVar a)
forkR p = do
  r <- new
  fork (p >>= put r)
  return r

test = do
  print (runP $ return 3)
  print (runP $ do r <- new; put r 3; get r)
  print (runP $ do r <- new; fork (put r 3); get r)
  print ((runP $ do r <- new; get r)  :: Int)

