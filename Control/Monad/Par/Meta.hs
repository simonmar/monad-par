{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}


-- The Meta scheduler which can be parameterized over various
-- "Resources", including:
--   * Serial execution
--   * Shared memory SMP
--   * GPU accelerators
--   * Remote Machine "accelerators" (i.e. distributed)


module Control.Monad.Par.Meta where

import Control.Applicative
import Control.Concurrent ( forkOn
                          , MVar
                          , newEmptyMVar
                          , putMVar
                          , readMVar
                          , takeMVar
                          , tryPutMVar
                          , QSem
                          , signalQSem
                          )
import Control.DeepSeq
import Control.Monad
import "mtl" Control.Monad.Cont (ContT(..), MonadCont, callCC, runContT)
import "mtl" Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.IO.Class
import Control.Exception (catch, throwTo, SomeException)
import GHC.Conc

import Data.Concurrent.Deque.Class (WSDeque)
import Data.Concurrent.Deque.Reference.DequeInstance
import Data.Concurrent.Deque.Reference as R
import Data.IntMap (IntMap)
-- import Data.Word   (Word64)
import qualified Data.IntMap as IntMap
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.IORef (IORef, writeIORef, newIORef)

import System.IO.Unsafe (unsafePerformIO)
import System.IO (stderr)
import System.Posix.Affinity (setAffinityOS)
import System.Random.MWC

import Text.Printf
import qualified Debug.Trace as DT

import Control.Monad.Par.Meta.Resources.Debugging (dbgTaggedMsg)
import Control.Monad.Par.Meta.HotVar.IORef
import qualified Control.Monad.Par.Class as PC

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

--------------------------------------------------------------------------------
-- Types

newtype Par a = Par { unPar :: ContT () ROnly a }
    deriving (Monad, MonadCont, MonadReader Sched, 
              MonadIO, Applicative, Functor, Typeable)
type ROnly = ReaderT Sched IO

newtype IVar a = IVar (HotVar (IVarContents a))

data IVarContents a = Full a | Empty | Blocked [a -> IO ()]

newtype InitAction = IA { runIA ::
    -- Combined 'StealAction' for the current scheduler.
     StealAction           
    -- The global structure of schedulers.
  -> HotVar (IntMap Sched) 
  -> IO ()
  }

instance Show InitAction where
  show _ = "<InitAction>"

instance Monoid InitAction where
  mempty = IA $ \_ _ -> return ()
  (IA ia1) `mappend` (IA ia2) = IA ia'
    where ia' sa schedMap = ia1 sa schedMap >> ia2 sa schedMap            
                             

newtype StealAction = SA { runSA ::
     -- 'Sched' for the current thread
     Sched
     -- Map of all 'Sched's
  -> HotVar (IntMap Sched)
  -> IO (Maybe (Par ()))
  }

instance Show StealAction where
  show _ = "<StealAction>"

instance Monoid StealAction where
  mempty = SA $ \_ _ -> return Nothing
  (SA sa1) `mappend` (SA sa2) = SA sa'
    where sa' sched schedMap = do
            mwork <- sa1 sched schedMap
            case mwork of
              Nothing -> sa2 sched schedMap
              _ -> return mwork                

data Sched = Sched 
    { 
      ---- Per capability ----
      no       :: {-# UNPACK #-} !Int,
      tids     :: HotVar (Set ThreadId),
      workpool :: WSDeque (Par ()),
      rng      :: HotVar GenIO, -- Random number gen for work stealing.
      mortals  :: HotVar Int, -- How many threads are mortal on this capability?

      -- | Are we on the *first* steal attempt (after doing productive
      --   work), or is this the Nth failed steal in a row?
      consecutiveFailures :: IORef Int,

      -- | A per-thread counter used for unique ivarIDs.  
      --   (Multiple by numCapabilities and add 'no' for uniqueness.)
      ivarUID :: HotVar Int,

      ---- Meta addition ----
      stealAction :: StealAction
    }

instance Show Sched where
  show Sched{ no } = printf "Sched{ no=%d }" no 

--------------------------------------------------------------------------------
-- Helpers

-- Exceptions that walk up the fork tree of threads:
forkWithExceptions :: (IO () -> IO ThreadId) -> String -> IO () -> IO ThreadId
forkWithExceptions forkit descr action = do 
   parent <- myThreadId
   forkit $ 
      Control.Exception.catch action
	 (\ e -> do
	  BS.hPutStrLn stderr $ BS.pack $ "Exception inside child thread "++show descr++": "++show e
	  throwTo parent (e::SomeException)
	 )

{-# INLINE ensurePinned #-}
-- Ensure that we execute an action within a pinned thread:
ensurePinned :: IO a -> IO a
ensurePinned action = do 
  tid <- myThreadId
  (cap, pinned) <- threadCapability tid  
  if pinned 
   then action 
   else do mv <- newEmptyMVar 
	   forkOn cap (action >>= putMVar mv)
	   takeMVar mv


--------------------------------------------------------------------------------
-- Work queue helpers

{-# INLINE popWork #-}
popWork :: Sched -> IO (Maybe (Par ()))
popWork Sched{ workpool, no } = do
  when dbg $ do
    (cap, _) <- threadCapability =<< myThreadId
    dbgTaggedMsg 4 $ BS.pack $ "[meta: cap "++show cap++ "] trying to pop local work on Sched "++ show no
  R.tryPopL workpool

{-# INLINE pushWork #-}
pushWork :: Sched -> Par () -> IO ()
pushWork Sched{ workpool } work = R.pushL workpool work

{-# INLINE pushWorkEnsuringWorker #-}
pushWorkEnsuringWorker :: Sched -> Par () -> IO (Maybe ())
pushWorkEnsuringWorker Sched { no } work = do
  let attempt n = do
        sched@Sched { no, tids } <- getSchedForCap n
        set <- readHotVar tids
        case Set.null set of
          False -> do
            when dbg $ printf "[meta] pushing ensured work onto cap %d\n" no
            Just <$> pushWork sched work
          True -> return Nothing
      loop []     = return Nothing
      loop (n:ns) = do
        msucc <- attempt n
        case msucc of
          Just () -> return $ Just ()
          Nothing -> loop ns
  msucc <- attempt no
  schedNos <- IntMap.keys <$> readHotVar globalScheds
  case msucc of
    Just () -> return $ Just ()
    Nothing -> loop schedNos
--------------------------------------------------------------------------------
-- Global structures and helpers for proper nesting behavior

{-# NOINLINE globalScheds #-}
globalScheds :: HotVar (IntMap Sched)
globalScheds = unsafePerformIO . newHotVar $ IntMap.empty

{-# NOINLINE startBarrier #-}
startBarrier :: MVar ()
startBarrier = unsafePerformIO newEmptyMVar

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
  sched <- Sched cap <$> newHotVar (Set.empty)  -- tids
                     <*> R.newQ                 -- workpool
                     <*> (newHotVar =<< create) -- rng
                     <*> newHotVar 0            -- mortals
                     <*> newIORef  0            -- consecutiveFailures
                     <*> newHotVar 0            -- ivarUID
                     <*> pure sa                -- stealAction
  modifyHotVar globalScheds $ \scheds ->
    case IntMap.lookup cap scheds of
      Just sched -> (scheds, sched)
      Nothing -> if dbg
                 then DT.trace (printf "[%d] created scheduler" cap)
                               (IntMap.insert cap sched scheds, sched)
                 else (IntMap.insert cap sched scheds, sched)

--------------------------------------------------------------------------------
-- Worker routines

forkOn' cap k = forkOn cap $ setAffinityOS cap >> k

-- | Spawn a pinned worker that will stay on a capability.
-- 
-- Note: this does not check for nesting, and should be called
-- appropriately. It is the caller's responsibility to manage things
-- like mortal counts.
spawnWorkerOnCap :: StealAction -> Int -> IO ThreadId
spawnWorkerOnCap sa cap = 
  forkWithExceptions (forkOn' cap) "spawned Par worker" $ do
    me <- myThreadId
    sched@Sched{ tids } <- makeOrGetSched sa cap
    modifyHotVar_ tids (Set.insert me)
    when dbg$ dbgTaggedMsg 2 $ BS.pack $
      printf "[meta: cap %d] spawning new worker" cap
    -- wait on the barrier to start
    readMVar startBarrier
    when dbg$ dbgTaggedMsg 2 $ BS.pack $ 
      printf "[meta: cap %d] new working entering loop" cap
    runReaderT (workerLoop 0 errK) sched

-- | Like 'spawnWorkerOnCap', but takes a 'QSem' which is signalled
-- just before the new worker enters the 'workerLoop'.
spawnWorkerOnCap' :: QSem -> StealAction -> Int -> IO ThreadId
spawnWorkerOnCap' qsem sa cap = 
  forkWithExceptions (forkOn' cap) "spawned Par worker" $ do
    me <- myThreadId
    sched@Sched{ tids } <- makeOrGetSched sa cap
    modifyHotVar_ tids (Set.insert me)
    when dbg$ dbgTaggedMsg 2 $ BS.pack $ "[meta: cap "++show cap++"] spawning new worker" 
    signalQSem qsem
    -- wait on the barrier to start
    readMVar startBarrier
    when dbg$ dbgTaggedMsg 2 $ BS.pack $ 
      printf "[meta: cap %d] new working entering loop" cap
    runReaderT (workerLoop 0 errK) sched

errK = error "this closure shouldn't be used"

reschedule :: Par a
reschedule = Par $ ContT (workerLoop 0)

workerLoop :: Int -> ignoredCont -> ROnly ()
workerLoop failCount _k = do
  mysched@Sched{ no, mortals, stealAction, consecutiveFailures } <- ask
  mwork <- liftIO $ popWork mysched
  case mwork of
    Just work -> do
      when dbg $ liftIO $ printf "[meta %d] popped work from own queue\n" no
      runContT (unPar work) $ const (workerLoop 0 _k)
    Nothing -> do
      -- check if we need to die
      die <- liftIO $ modifyHotVar mortals $ \ms ->
               case ms of
                 0         -> (0, False)                              
                 n | n > 0 -> (n-1, True)
                 n         -> error $
                   printf "unexpected mortals count %d on cap %d" n no
      unless die $ do
        -- Before steal we make sure the consecutiveFailures field is up
        -- to date.  This would seem to be a very ugly method of
        -- passing an extra argument to the steal action, and if we
        -- could tolerate it, it should perhaps become an additional argument:
        liftIO$ writeIORef consecutiveFailures failCount
        mwork <- liftIO (runSA stealAction mysched globalScheds)
        case mwork of
          Just work -> runContT (unPar work) $ const (workerLoop 0 _k)
          Nothing -> do 
            when dbg $ liftIO $ dbgTaggedMsg 4 $ BS.pack $ "[meta: cap "++show no++"] failed to find work; looping" 
            workerLoop (failCount + 1) _k 

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
runMetaParIO ia sa work = ensurePinned $ 
  do
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
          dbgTaggedMsg 2 $ BS.pack "[meta] runMetaParIO computation finished, putting final MVar..."
          putMVar ansMVar ans
          -- if we're nested, we need to shut down the extra thread on
          -- our capability. If non-nested, we're done with the whole
          -- thing, and should really shut down.
          modifyHotVar_ mortals (1+)
          dbgTaggedMsg 2 $ BS.pack "[meta] runMetaParIO: done putting mvar and incrementing mortals."

  -- determine whether this is a nested call
  isNested <- Set.member tid <$> readHotVar tids
  if isNested then
        -- if it is, we need to spawn a replacement worker while we wait on ansMVar
        void $ spawnWorkerOnCap sa cap
        -- if it's not, we need to run the init action 
   else runIA ia sa globalScheds

  -- push the work, and then wait for the answer
  msucc <- pushWorkEnsuringWorker sched wrappedComp
  when (msucc == Nothing)
    $ error "[meta] could not find a scheduler with an active worker!"
  dbgTaggedMsg 2 $ BS.pack
    "[meta] runMetaParIO: Work pushed onto queue, now waiting on final MVar..."
  -- trigger the barrier so that workers start
  _ <- tryPutMVar startBarrier ()
  ans <- takeMVar ansMVar

  -- TODO: Invariant checking -- make sure there is no work left:
  -- sanityCheck

  return ans

{-# INLINE runMetaPar #-}
runMetaPar :: InitAction -> StealAction -> Par a -> a
runMetaPar ia sa work = unsafePerformIO $ runMetaParIO ia sa work

--------------------------------------------------------------------------------
-- Boilerplate

spawnP :: NFData a => a -> Par (IVar a)
spawnP = spawn . return

instance PC.ParFuture Par IVar where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar Par IVar where
  fork = fork
  new  = new
  put_ = put_
