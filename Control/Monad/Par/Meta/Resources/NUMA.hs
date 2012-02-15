{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

module Control.Monad.Par.Meta.Resources.NUMA (
    initAction
  , initActionFromEnv
  , stealAction
  , stealActionFromEnv
  ) where

import Control.Monad

import qualified Data.IntMap as IntMap
import qualified Data.Vector as Vector

import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import System.Random.MWC

import Text.Printf

import Control.Monad.Par.Meta hiding (dbg, stealAction)
import Control.Monad.Par.Meta.HotVar.IORef
import qualified Control.Monad.Par.Meta.Resources.SharedMemory as SharedMemory

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

-- | A 'SimpleTopology is a list of lists of capabilities, where each
-- nested list represents the capabilities in a NUMA node.
type SimpleTopology = [[Int]]

-- | 'InitAction' for spawning a NUMA scheduler.
initAction :: SimpleTopology -> InitAction
initAction topo sa _m = do
  when dbg $ printf "NUMA scheduler spawning subordinate schedulers\n"
  forM_ topo $ \node -> SharedMemory.initActionForCaps node sa _m

-- | A version of 'initAction' that reads a 'SimpleTopology' from the
-- @NUMA_TOPOLOGY@ environment variable. For example,
-- @NUMA_TOPOLOGY='[[1,2],[3,4]]'@ configures a 2x2 system.
initActionFromEnv :: InitAction
initActionFromEnv = initAction topoFromEnv

{-# NOINLINE topoFromEnv #-}
topoFromEnv :: SimpleTopology
topoFromEnv = unsafePerformIO $ do
  topoStr <- getEnv "NUMA_TOPOLOGY"
  let topo = read topoStr :: SimpleTopology
  when dbg $ printf "Read NUMA_TOPOLOGY=%s\n" (show topo)
  return topo

{-# INLINE randModN #-}
randModN :: Int -> HotVar GenIO -> IO Int
randModN n rngRef = uniformR (0, n-1) =<< readHotVar rngRef

-- | Given a 'SimpleTopology' and a number of steals to attempt per
-- invocation, return a 'StealAction'.
stealAction :: SimpleTopology -> Int -> StealAction
stealAction topo numTries = sa
  where
    numNodes = length topo
    triesPerNode = numTries `quot` numNodes
    buildSteal caps = 
      SharedMemory.stealActionForCaps caps triesPerNode
    subSteals = map buildSteal topo
    capAssocs = [map (\cap -> (cap, sa)) caps | caps <- topo | sa <- subSteals]
    capMap = IntMap.fromList (concat capAssocs)
    saVec = Vector.fromList subSteals
    sa :: StealAction
    sa sched@Sched { no, rng } schedsRef = do
      -- first, steal from the scheduler for this clique
      let cliqueSteal = IntMap.lookup no capMap
      case cliqueSteal of
        Nothing -> error $ printf "[%d] no steal action for NUMA node" no
        Just sa -> do
          mtask <- sa sched schedsRef
          case mtask of
            jtask@(Just _) -> return jtask
            Nothing -> do
              let getNext :: IO Int
                  getNext = randModN numNodes rng
                  -- | Main steal loop
                  loop :: Int -> Int -> IO (Maybe (Par ()))
                  loop 0 _ = return Nothing      
                  loop n i = do
                    -- unlike shared memory, ok to steal from "self"
                    mtask <- (saVec Vector.! i) sched schedsRef
                    maybe (loop (n-1) =<< getNext) (return . return) mtask
              loop numTries =<< getNext

stealActionFromEnv :: Int -> StealAction
stealActionFromEnv = stealAction topoFromEnv