{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

-- The Meta scheduler which can be parameterized over various
-- "Resources", including:
--   * Serial execution
--   * Shared memory SMP
--   * GPU accelerators
--   * Remote Machine "accelerators" (i.e. distributed)


module Control.Monad.Par.Meta 
( 
-- * Core Meta-Par types
  Par
, IVar
-- * Operations
, PC.ParFuture(..)
, PC.ParIVar(..)
-- * Entrypoints
, runMetaPar
, runMetaParIO
-- * Implementation API
, Sched(..)
, GlobalState
-- ** Execution Resources
, Resource(..)
, Startup(..)
, WorkSearch(..)
-- ** Utilities
, forkWithExceptions
, spawnWorkerOnCPU
) where

import Control.Applicative
import Control.Concurrent ( MVar
                          , newEmptyMVar
                          , putMVar
                          , readMVar
                          , takeMVar
                          , tryPutMVar
                          , tryTakeMVar
                          )
import Control.DeepSeq
import Control.Monad
import "mtl" Control.Monad.Cont (ContT(..), MonadCont, callCC, runContT)
import "mtl" Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.IO.Class
import Control.Exception (catch, throwTo, SomeException)

import Data.Concurrent.Deque.Class (WSDeque)
import Data.Concurrent.Deque.Reference.DequeInstance ()
import Data.Concurrent.Deque.Reference as R
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.IORef (IORef, writeIORef, newIORef)
import Data.Vector (Vector)
import qualified Data.Vector as V

import System.IO.Unsafe (unsafePerformIO)
import System.IO (stderr)
import System.Random.MWC

import Text.Printf
import qualified Debug.Trace as DT

#ifdef AFFINITY
import System.Posix.Affinity (setAffinityOS)
#endif

import Control.Monad.Par.Meta.Resources.Debugging (dbgTaggedMsg)
import Control.Monad.Par.Meta.HotVar.IORef
import qualified Control.Monad.Par.Class as PC

#if __GLASGOW_HASKELL__ >= 702
import GHC.Conc (forkOn, ThreadId, myThreadId, threadCapability)
import Control.Concurrent (getNumCapabilities)
threadCapability' tid = Just <$> threadCapability tid
#else
import GHC.Conc (forkOnIO, ThreadId, myThreadId, numCapabilities)
forkOn :: Int -> IO () -> IO ThreadId
forkOn = forkOnIO
getNumCapabilities :: IO Int
getNumCapabilities = return numCapabilities

-- This is a best-effort recreation of threadCapability for older GHC
-- versions that lack it. If the calling thread is a meta-par worker,
-- it must have been spawned with forkOn, so we return an answer that
-- indicates it is pinned.
-- 
-- It does a search through the ThreadIds stored in the global
-- scheduler structure, and if it finds a match for the current
-- thread, it returns the 'no' field associated with that
-- ThreadId. Otherwise it returns Nothing.
threadCapability' tid = do
  vec <- readHotVar globalScheds
  let f Nothing = return $ mempty
      f (Just Sched{ no, tids }) = do
        set <- readHotVar tids
        case Set.member tid set of
          False -> return $ mempty
          True -> return $ First (Just no)
  cap <- getFirst . mconcat <$> mapM f (V.toList vec)
  return ((,True) <$> cap)
#endif
threadCapability' :: ThreadId -> IO (Maybe (Int, Bool))


#if __GLASGOW_HASKELL__ < 700
void = fmap (const ())
#endif


dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif


--------------------------------------------------------------------------------
-- Types

-- | The Meta-Par monad with its full suite of instances. Note that
-- the 'MonadIO' instance, while essential for building new
-- 'Resource's, is unsafe in client code when combined with
-- 'runMetaPar'. This type should therefore be exposed to client code
-- as a @newtype@ that omits the 'MonadIO' instance.
newtype Par a = Par { unPar :: ContT () ROnly a }
    deriving (Monad, MonadCont, MonadReader Sched, 
              MonadIO, Applicative, Functor, Typeable)

type ROnly = ReaderT Sched IO

-- | An 'IVar' is a /write-once/, /read-many/ structure for
-- communication between 'Par' threads.
newtype IVar a = IVar (HotVar (IVarContents a))

instance NFData (IVar a) where
  rnf _ = ()

data IVarContents a = Full a | Empty | Blocked [a -> IO ()]

-- | A 'GlobalState' structure tracks the state of all Meta-Par
-- workers in a program in a 'Data.Vector' indexed by capability
-- number.
type GlobalState = Vector (Maybe Sched)

-- | The 'Startup' component of a 'Resource' is a callback that
-- implements initialization behavior. For example, the SMP 'Startup'
-- calls 'spawnWorkerOnCPU' a number of times. The arguments to
-- 'Startup' are the combined 'Resource' of the current scheduler and
-- a thread-safe reference to the 'GlobalState'.
newtype Startup = St { runSt ::
     WorkSearch 
  -> HotVar GlobalState 
  -> IO () 
  }

instance Show Startup where
  show _ = "<Startup>"

instance Monoid Startup where
  mempty = St $ \_ _ -> return ()
  (St st1) `mappend` (St st2) = St st'
    where st' ws schedMap = st1 ws schedMap >> st2 ws schedMap            
                             
-- | The 'WorkSearch' component of a 'Resource' is a callback that
-- responds to requests for work from Meta-Par workers. The arguments
-- to 'WorkSearch' are the 'Sched' for the current thread and a
-- thread-safe reference to the 'GlobalState'.
newtype WorkSearch = WS { runWS ::
     Sched
  -> HotVar GlobalState
  -> IO (Maybe (Par ()))
  }

instance Show WorkSearch where
  show _ = "<WorkSearch>"

instance Monoid WorkSearch where
  mempty = WS $ \_ _ -> return Nothing
  (WS ws1) `mappend` (WS ws2) = WS ws'
    where ws' sched schedMap = do
            mwork <- ws1 sched schedMap
            case mwork of
              Nothing -> ws2 sched schedMap
              _ -> return mwork                

-- | A 'Resource' provides an abstraction of heterogeneous execution
-- resources, and may be combined using 'Data.Monoid'
-- operations. Composition of resources is left-biased; for example,
-- if @resource1@ always returns work from its 'WorkSearch', the
-- composed resource @resource1 `mappend` resource2@ will never
-- request work from @resource2@.
data Resource = Resource {
    startup  :: Startup
  , workSearch :: WorkSearch
  } deriving (Show)

instance Monoid Resource where
  mempty = Resource mempty mempty
  Resource st1 ws1 `mappend` Resource st2 ws2 =
    Resource (st1 `mappend` st2) (ws1 `mappend` ws2)

data Sched = Sched 
    { 
      ---- Per capability ----
      -- | Capability number
      no       :: {-# UNPACK #-} !Int,
      -- | The 'ThreadId's of all worker threads on this capability
      tids     :: HotVar (Set ThreadId),
      -- | The local 'WSDeque' for this worker. The worker may push
      -- and pop from the left of its own 'workpool', but workers on
      -- other threads may only steal from the right.
      workpool :: WSDeque (Par ()),
      -- | A 'GenIO' for random work stealing.
      rng      :: HotVar GenIO,
      -- | A counter of how many extra workers are working on this
      -- capability. This situation arises during nested calls to
      -- 'runMetaPar', and the worker loop kills workers as necessary
      -- to keep this value at @1@.
      mortals  :: HotVar Int, 

      -- | Tracks the number of consecutive times this worker has
      -- invoked a 'WorkSearch' and received 'Nothing'. This is used
      -- to implement backoff in
      -- 'Control.Monad.Par.Meta.Resources.Backoff'.
      consecutiveFailures :: IORef Int,

      -- | A per-thread source of unique identifiers for
      -- 'IVar's. Multiply this value by 'getNumCapabilities' and add
      -- 'no' for uniqueness.
      ivarUID :: HotVar Int,

      ---- Meta addition ----
      -- | The 'WorkSearch' of this worker's associated 'Resource'.
      schedWs :: WorkSearch
    }

instance Show Sched where
  show Sched{ no } = printf "Sched{ no=%d }" no 

--------------------------------------------------------------------------------
-- Helpers

-- | Produces a variant of 'forkOn' that allows exceptions from child
-- threads to propagate up to the parent thread.
forkWithExceptions :: (IO () -> IO ThreadId) -- ^ The basic 'forkOn' implementation
                   -> String -- ^ A name for the child thread in error messages
                   -> (IO () -> IO ThreadId)
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
  mp <- threadCapability' tid  
  case mp of
    Just (_, True) -> action
    Just (cap, _ ) -> do
      mv <- newEmptyMVar 
      void $ forkOn cap (action >>= putMVar mv)
      takeMVar mv
    Nothing -> do
      -- Older GHC case: we only consider a thread pinned if it's one
      -- of the threads manaaged by the global sched state. If it's
      -- not, we have no choice but to assume that it's not pinned,
      -- and spawn a new thread on CPU 0, which is an arbitrary
      -- choice.
      mv <- newEmptyMVar 
      void $ forkOn 0 (action >>= putMVar mv)
      takeMVar mv

--------------------------------------------------------------------------------
-- Work queue helpers

{-# INLINE popWork #-}
popWork :: Sched -> IO (Maybe (Par ()))
popWork Sched{ workpool, no } = do
  when dbg $ do
#if __GLASGOW_HASKELL__ >= 702
    (cap, _) <- threadCapability =<< myThreadId
    dbgTaggedMsg 4 $ BS.pack $ "[meta: cap "++show cap++ "] trying to pop local work on Sched "++ show no
#else
    dbgTaggedMsg 4 $ BS.pack $ "[meta] trying to pop local work on Sched "++ show no
#endif
  R.tryPopL workpool

{-# INLINE pushWork #-}
pushWork :: Sched -> Par () -> IO ()
pushWork Sched{ workpool } work = R.pushL workpool work

{-# INLINE pushWorkEnsuringWorker #-}
pushWorkEnsuringWorker :: Sched -> Par () -> IO (Maybe ())
pushWorkEnsuringWorker _ work = do
  no <- takeMVar workerPresentBarrier
  sched@Sched { tids } <- getSchedForCap no
  set <- readHotVar tids
  case Set.null set of
    False -> do
      when dbg $ printf "[meta] pushing ensured work onto cap %d\n" no
      Just <$> pushWork sched work
    True -> error $ printf "[meta] worker barrier filled by non-worker %d\n" no
  

{-
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
-}

--------------------------------------------------------------------------------
-- Global structures and helpers for proper nesting behavior

{-# NOINLINE globalScheds #-}
globalScheds :: HotVar GlobalState
globalScheds = unsafePerformIO $ do
  n <- getNumCapabilities
  newHotVar $ V.replicate n Nothing

{-# NOINLINE workerPresentBarrier #-}
-- | Starts empty. Each new worker spawned tries to put its CPU
-- number. 'pushWorkEnsuringWorker' waits on this to ensure it pushes
-- the initial computation on a CPU with a worker.
workerPresentBarrier :: MVar Int
workerPresentBarrier = unsafePerformIO newEmptyMVar

{-# NOINLINE startBarrier #-}
startBarrier :: MVar ()
startBarrier = unsafePerformIO newEmptyMVar

-- | Warning: partial!
getSchedForCap :: Int -> IO Sched
getSchedForCap cap = do
  scheds <- readHotVar globalScheds
  case scheds V.! cap of
    Just sched -> return sched
    Nothing -> error $ 
      printf "tried to get a Sched for capability %d before initializing" cap


makeOrGetSched :: WorkSearch -> Int -> IO Sched
makeOrGetSched ws cap = do
  sched <- Sched cap <$> newHotVar (Set.empty)  -- tids
                     <*> R.newQ                 -- workpool
                     <*> (newHotVar =<< create) -- rng
                     <*> newHotVar 0            -- mortals
                     <*> newIORef  0            -- consecutiveFailures
                     <*> newHotVar 0            -- ivarUID
                     <*> pure ws                -- workSearch
  modifyHotVar globalScheds $ \scheds ->
    case scheds V.! cap of
      Just sched -> (scheds, sched)
      Nothing -> if dbg
                 then DT.trace (printf "[%d] created scheduler" cap)
                               (scheds V.// [(cap, Just sched)], sched)
                 else (scheds V.// [(cap, Just sched)], sched)

--------------------------------------------------------------------------------
-- Worker routines

forkOn' :: Int -> IO () -> IO ThreadId
#ifdef AFFINITY
forkOn' cap k = forkOn cap $ setAffinityOS cap >> k
#else
forkOn' = forkOn
#endif

-- | Spawn a Meta-Par worker that will stay on a given capability.
-- 
-- Note: this does not check whether workers already exist on the
-- capability, and should be called appropriately. In particular, it
-- is the caller's responsibility to manage things like the 'mortal'
-- count of the given capability.
spawnWorkerOnCPU :: WorkSearch -- ^ The 'WorkSearch' called by the new worker
                 -> Int        -- ^ Capability
                 -> IO ThreadId
spawnWorkerOnCPU ws cap = 
  forkWithExceptions (forkOn' cap) "spawned Par worker" $ do
    me <- myThreadId
    sched@Sched{ tids } <- makeOrGetSched ws cap
    modifyHotVar_ tids (Set.insert me)
    when dbg$ dbgTaggedMsg 2 $ BS.pack $
      printf "[meta: cap %d] spawning new worker" cap
    -- at least this worker is ready, so try filling the MVar
    _ <- tryPutMVar workerPresentBarrier cap
    -- wait on the barrier to start
    readMVar startBarrier
    when dbg$ dbgTaggedMsg 2 $ BS.pack $ 
      printf "[meta: cap %d] new working entering loop" cap
    runReaderT (workerLoop 0 errK) sched

errK :: a
errK = error "this closure shouldn't be used"

reschedule :: Par a
reschedule = Par $ ContT (workerLoop 0)

workerLoop :: Int -> ignoredCont -> ROnly ()
workerLoop failCount _k = do
  mysched@Sched{ no, mortals, schedWs=ws, consecutiveFailures } <- ask
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
        mwork <- liftIO (runWS ws mysched globalScheds)
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
get (IVar hv) = callCC $ \cont -> do
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
put_ (IVar hv) !content = do
  liftIO $ do
    ks <- modifyHotVar hv $ \contents ->
      case contents of
        Empty      -> (Full content, [])
        Blocked ks -> (Full content, ks)
        Full _      -> error "multiple put"
    mapM_ ($content) ks

{-# INLINE put #-}
put :: NFData a => IVar a -> a -> Par ()
put iv a = deepseq a (put_ iv a)

{-# INLINE spawn #-}
spawn :: NFData a => Par a -> Par (IVar a)
spawn p = do r <- new; fork (p >>= put  r); return r

{-# INLINE spawn_ #-}
spawn_ :: Par a -> Par (IVar a)
spawn_ p = do r <- new; fork (p >>= put_ r); return r


--------------------------------------------------------------------------------
-- Entrypoint

-- | Run a 'Par' computation in the 'IO' monad, allowing
-- non-deterministic Meta-Par variants to be safely executed.
runMetaParIO :: Resource -> Par a -> IO a
runMetaParIO Resource{ startup=st, workSearch=ws } work = ensurePinned $ 
  do
  -- gather information
  tid <- myThreadId
  mp <- threadCapability' tid
  let cap = case mp of
              Just (n, _) -> n
              -- Older GHC case: default to CPU 0 if the calling
              -- thread is not already managed by meta-par
              Nothing -> 0

  sched@Sched{ tids, mortals } <- makeOrGetSched ws cap

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
        void $ spawnWorkerOnCPU ws cap
        -- if it's not, we need to run the init action 
   else runSt st ws globalScheds

  -- push the work, and then wait for the answer
  msucc <- pushWorkEnsuringWorker sched wrappedComp
  when (msucc == Nothing)
    $ error "[meta] could not find a scheduler with an active worker!"
  dbgTaggedMsg 2 $ BS.pack
    "[meta] runMetaParIO: Work pushed onto queue, now waiting on final MVar..."
  -- trigger the barrier so that workers start
  _ <- tryPutMVar startBarrier ()
  -- make sure the worker barrier is clear for subsequent runPars
  _ <- tryTakeMVar workerPresentBarrier
  ans <- takeMVar ansMVar

  -- TODO: Invariant checking -- make sure there is no work left:
  -- sanityCheck

  return ans

{-# INLINE runMetaPar #-}
-- | Run a 'Par' computation, and return its result as a pure
-- value. If the choice of 'Resource' introduces non-determinism, use
-- 'runMetaParIO' instead, as non-deterministic computations are not
-- referentially-transparent.
runMetaPar :: Resource -> Par a -> a
runMetaPar rsrc work = unsafePerformIO $ runMetaParIO rsrc work

--------------------------------------------------------------------------------
-- Boilerplate

spawnP :: NFData a => a -> Par (IVar a)
spawnP = spawn . return

instance PC.ParFuture IVar Par where
  get    = get
  spawn  = spawn
  spawn_ = spawn_
  spawnP = spawnP

instance PC.ParIVar IVar Par where
  fork = fork
  new  = new
  put_ = put_
