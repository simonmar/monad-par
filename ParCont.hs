{-# LANGUAGE RankNTypes, NamedFieldPuns, BangPatterns #-}

module ParCont (
    runP, -- :: P a -> a
    new,  -- :: P (Var a)
    get,  -- :: Var a -> P a
    put,  -- :: Var a -> a -> P a
    fork, -- :: P a -> P a
  ) where

import Control.Monad.Reader as R
import Control.Monad.Cont as C
import Control.Monad
import Data.IORef
import System.IO.Unsafe

-- ---------------------------------------------------------------------------

data Sched = Sched 
    { workpool :: IORef [P ()],
      myid :: Int
    }
--  deriving Show

type SchedR = R.ReaderT Sched IO
type P a = C.ContT () SchedR a

type M r a = C.ContT r (R.ReaderT Sched IO) a

newtype PVar a = PVar (IORef (Either a [a -> P ()]))

mycallCC :: ((forall b . a -> M r b) -> M r a) -> M r a
mycallCC f = C.ContT $ \c -> C.runContT (f (\a -> C.ContT $ \_ -> c a)) c

runP :: P a -> a
runP x = unsafePerformIO $ do
  workpool <- newIORef []
  let state = Sched { workpool, myid = 1 }
  r <- newIORef (error "Uninitialized graph result read prematurely")
  R.runReaderT (C.runContT x (C.liftIO . writeIORef r)) state
  readIORef r

-- -----------------------------------------------------------------------------

pushWork :: P () -> P ()
pushWork a = do
  Sched { workpool } <- R.ask
  C.liftIO $ do
    xs <- readIORef workpool
    writeIORef workpool (a:xs)

popWork  :: SchedR (Maybe (P ()))
popWork  = do
   Sched { workpool } <- R.ask
   R.lift $ do
     xs <- readIORef workpool
     case xs of
       [] -> return Nothing
       x:xs' -> do
         writeIORef workpool xs'
         return (Just x)

-- -----------------------------------------------------------------------------

get :: PVar a -> P a
get (PVar r) =
  mycallCC $ \cont -> do
    r <- C.liftIO $ do
      m <- readIORef r
      case m of
        Left v -> return (cont v)
        Right waiters -> do
           writeIORef r $! Right (cont:waiters)
           return reschedule
    -- Out of the transaction, bounce on that trampoline:
    r

reschedule :: P a
reschedule = C.ContT rescheduleR

rescheduleR :: a -> R.ReaderT Sched IO ()
rescheduleR _ = do
  m <- popWork 
  case m of
    Nothing -> return ()
    Just (C.ContT f)  -> f rescheduleR

fork :: P () -> P ()
fork p = pushWork p

new :: P (PVar a)
new = C.liftIO $ newIORef (Right []) >>= return . PVar

put :: PVar a -> a -> P ()
put (PVar r) (!v) = do
  waiters <- C.liftIO $ do
    m <- readIORef r
    case m of
      Left v -> error "multiple put"
      Right waiters -> return waiters
  C.liftIO $ writeIORef r (Left v)
  mapM_ (pushWork . ($v)) waiters
