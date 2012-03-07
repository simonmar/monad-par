{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind -fno-warn-name-shadowing #-}

module Control.Monad.Par.Meta.Resources.NUMA (
    defaultInit
  , defaultSteal
  , initForTopo
  , stealForTopo
  , mkResource
  ) where

import Control.Monad

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

-- | If no topology given, reads @NUMA_TOPOLOGY@ env variable.
mkResource :: Maybe SimpleTopology -> Int -> Resource
mkResource Nothing     tries =
  Resource defaultInit (defaultSteal tries)
mkResource (Just topo) tries = 
  Resource (initForTopo topo) (stealForTopo topo tries)

-- | A 'SimpleTopology is a list of lists of capabilities, where each
-- nested list represents the capabilities in a NUMA node.
type SimpleTopology = [[Int]]

-- | 'InitAction' for spawning a NUMA scheduler.
initForTopo :: SimpleTopology -> InitAction
initForTopo topo = IA ia
  where ia sa _m = do
          when dbg $ printf "NUMA scheduler spawning subordinate schedulers\n"
          forM_ topo $ \node -> 
            runIA (SharedMemory.initForCaps node) sa _m

-- | A version of 'initAction' that reads a 'SimpleTopology' from the
-- @NUMA_TOPOLOGY@ environment variable. For example,
-- @NUMA_TOPOLOGY='[[1,2],[3,4]]'@ configures a 2x2 system.
defaultInit :: InitAction
defaultInit = initForTopo topoFromEnv

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
stealForTopo :: SimpleTopology -> Int -> StealAction
stealForTopo topo numTries = SA sa
  where
    numNodes = length topo
    triesPerNode = numTries `quot` numNodes
    buildSteal caps = 
      SharedMemory.stealForCaps caps triesPerNode
    subSteals = map buildSteal topo
    capAssocs = concat [map (\cap -> (cap, sa)) caps
                       | caps <- topo
                       | sa <- subSteals]
    lookupSA i =
      case lookup i capAssocs of
        Nothing -> error $ printf "accessed invalid capability %d" i
        Just sa -> sa
    capVec = Vector.generate (1 + maximum (map fst capAssocs)) lookupSA
    saVec = Vector.fromList subSteals
    sa sched@Sched { no, rng } schedsRef = do
      -- first, steal from the scheduler for this clique
      let sa = capVec Vector.! no
      mtask <- runSA sa sched schedsRef
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
                mtask <- runSA (saVec Vector.! i) sched schedsRef
                maybe (loop (n-1) =<< getNext) (return . return) mtask
          loop numTries =<< getNext

defaultSteal :: Int -> StealAction
defaultSteal = stealForTopo topoFromEnv