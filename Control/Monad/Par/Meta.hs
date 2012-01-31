{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}

module Control.Monad.Par.Meta where

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import "mtl" Control.Monad.Cont (ContT(..), MonadCont, callCC, runContT)
import "mtl" Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.IO.Class
import GHC.Conc

import Data.Concurrent.Deque.Class (WSDeque)
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Concurrent.Deque.Reference as R
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Set (Set)
import qualified Data.Set as Set

import System.Random
import System.IO.Unsafe (unsafePerformIO)

import Text.Printf

import Control.Monad.Par.Meta.HotVar.IORef

--------------------------------------------------------------------------------
-- Types

newtype Par a = Par { unPar :: ContT () ROnly a }
    deriving (Monad, MonadCont, MonadReader Sched, 
              MonadIO, Applicative, Functor)
type ROnly = ReaderT Sched IO

newtype IVar a = IVar (HotVar (IVarContents a))

data IVarContents a = Full a | Empty | Blocked [a -> IO ()]

type InitAction  = HotVar (IntMap Sched) -> IO ()
type StealAction =  Sched                 -- ^ 'Sched' for the current thread
                 -> HotVar (IntMap Sched) -- ^ Map of all 'Sched's
                 -> IO (Maybe (Par ()))

data Sched = Sched 
    { 
      ---- Per capability ----
      no       :: {-# UNPACK #-} !Int,
      tids     :: HotVar (Set ThreadId),
      workpool :: WSDeque (Par ()),
      rng      :: HotVar StdGen, -- Random number gen for work stealing.
      mortals  :: HotVar Int, -- How many threads are mortal on this capability?

      ---- Meta addition ----
      stealAction :: StealAction
    }

instance Show Sched where
  show Sched{ no } = printf "Sched{ no=%d }" no 

--------------------------------------------------------------------------------
-- Work queue helpers

{-# INLINE popWork #-}
popWork :: Sched -> IO (Maybe (Par ()))
popWork Sched{ workpool } = R.tryPopL workpool

{-# INLINE pushWork #-}
pushWork :: Sched -> Par () -> IO ()
pushWork Sched{ workpool } work = R.pushL workpool work

--------------------------------------------------------------------------------
-- Global structure and helpers for proper nesting behavior

{-# NOINLINE globalScheds #-}
globalScheds :: HotVar (IntMap Sched)
globalScheds = unsafePerformIO . newHotVar $ IntMap.empty

-- | Warning: partial!
getSchedForCap :: Int -> IO Sched
getSchedForCap cap = do
  msched <- IntMap.lookup cap <$> readHotVar globalScheds
  case msched of
    Just sched -> return sched
    Nothing -> error $ 
      printf "tried to get a Sched for capability %d before initializing" cap


makeOrGetSched :: StealAction -> Int -> IO Sched
makeOrGetSched sa cap = do
  sched <- Sched cap <$> newHotVar (Set.empty)
                     <*> R.newQ
                     <*> (newHotVar =<< newStdGen)
                     <*> newHotVar 0
                     <*> pure sa
  modifyHotVar globalScheds $ \scheds ->
    case IntMap.lookup cap scheds of
      Just sched -> (scheds, sched)
      Nothing -> (IntMap.insert cap sched scheds, sched)

--------------------------------------------------------------------------------
-- Worker routines

-- | Note: this does not check for nesting, and should be called
-- appropriately. It is the caller's responsibility to manage things
-- like mortal counts.
spawnWorkerOnCap :: StealAction -> Int -> IO ThreadId
spawnWorkerOnCap sa cap = forkOn cap $ do
  me <- myThreadId
  sched@Sched{ tids } <- makeOrGetSched sa cap
  modifyHotVar_ tids (Set.insert me)
  runReaderT (workerLoop errK) sched

errK = error "this closure shouldn't be used"

reschedule :: Par a
reschedule = Par $ ContT workerLoop

workerLoop :: ignoredCont -> ROnly ()
workerLoop _k = do
  mysched@Sched{ no, mortals, stealAction } <- ask
  mwork <- liftIO $ popWork mysched
  case mwork of
    Just work -> runContT (unPar work) $ const (workerLoop _k)
    Nothing -> do
      -- check if we need to die
      die <- liftIO $ modifyHotVar mortals $ \ms ->
               case ms of
                 0         -> (0, False)                              
                 n | n > 0 -> (n-1, True)
                 n         -> error $
                   printf "unexpected mortals count %d on cap %d" n no
      unless die $ do
        mwork <- liftIO (stealAction mysched globalScheds)
        case mwork of
          Just work -> runContT (unPar work) $ const (workerLoop _k)
          Nothing -> do
            -- idle behavior might go here
            workerLoop _k

{-# INLINE fork #-}
fork :: Par () -> Par ()
fork child = do
  sched <- ask
  callCC $ \parent -> do
    let wrapped = parent ()
    liftIO $ pushWork sched wrapped
    child >> reschedule

--------------------------------------------------------------------------------
-- IVar actions

{-# INLINE new #-}
new :: Par (IVar a)
new = liftIO $ IVar <$> newHotVar Empty

{-# INLINE get #-}
get :: IVar a -> Par a
get iv@(IVar hv) = callCC $ \cont -> do
  contents <- liftIO $ readHotVar hv
  case contents of
    Full a -> return a
    _ -> do
      sch <- ask
      join . liftIO $ modifyHotVar hv $ \contents ->
        case contents of
          Empty      -> (Blocked [pushWork sch . cont]     , reschedule)
          Blocked ks -> (Blocked (pushWork sch . cont : ks), reschedule)
          Full a     -> (Full a                            , return a)

{-# INLINE put_ #-}
put_ :: IVar a -> a -> Par ()
put_ iv@(IVar hv) !content = do
  sch <- ask
  liftIO $ do
    ks <- modifyHotVar hv $ \contents ->
      case contents of
        Empty      -> (Full content, [])
        Blocked ks -> (Full content, ks)
        Full _      -> error "multiple put"
    mapM_ ($content) ks

{-# INLINE put #-}
put iv a = deepseq a (put_ iv a)

{-# INLINE spawn #-}
spawn  p = do r <- new; fork (p >>= put  r); return r
{-# INLINE spawn_ #-}
spawn_ p = do r <- new; fork (p >>= put_ r); return r

--------------------------------------------------------------------------------
-- Entrypoint

runMetaParIO :: InitAction -> StealAction -> Par a -> IO a
runMetaParIO ia sa work = do
  -- gather information
  tid <- myThreadId
  (cap, _) <- threadCapability tid
  sched@Sched{ tids, mortals } <- makeOrGetSched sa cap

  -- make the MVar for this answer, and wrap the incoming work, and
  -- push it on the current scheduler
  ansMVar <- newEmptyMVar
  let wrappedComp = do 
        ans <- work
        liftIO $ do
          putMVar ansMVar ans
          -- if we're nested, we need to shut down the extra thread on
          -- our capability. If non-nested, we're done with the whole
          -- thing, and should really shut down.
          modifyHotVar_ mortals (1+)
  pushWork sched wrappedComp

  -- determine whether this is a nested call
  isNested <- Set.member tid <$> readHotVar tids
  -- if it's not, we need to run the init action
  unless isNested (ia globalScheds)
  -- if it is, we need to spawn a replacement worker while we wait on ansMVar
  when isNested (void $ spawnWorkerOnCap sa cap)
  -- wait for the answer
  ans <- takeMVar ansMVar
  return ans

{-# INLINE runMetaPar #-}
runMetaPar :: InitAction -> StealAction -> Par a -> a
runMetaPar ia sa work = unsafePerformIO $ runMetaParIO ia sa work