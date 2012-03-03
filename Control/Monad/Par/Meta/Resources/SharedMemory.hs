{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Control.Monad.Par.Meta.Resources.SharedMemory (
    initAction
  , initActionForCaps
  , stealAction
  , stealActionForCaps
  ) where

import Control.Concurrent
import Control.Monad

import Data.Concurrent.Deque.Reference as R
import qualified Data.IntMap as IntMap
import Data.List
import qualified Data.Vector as Vector

import System.Environment (getEnvironment)
import System.IO.Unsafe
import System.Posix.Affinity
import System.Random.MWC

import Text.Printf

import Control.Monad.Par.Meta hiding (dbg, stealAction)
import Control.Monad.Par.Meta.HotVar.IORef

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

{-# NOINLINE getCaps #-}
getCaps :: [Int]
getCaps = unsafePerformIO $ do
  env <- getEnvironment
  case lookup "SMP_CAPS" env of
    Just cs -> do
      when dbg $ printf "[SMP] initialized with capability list %s\n" 
                   (show ((read cs) :: [Int]))
      return $ read cs
    Nothing -> do 
      n <- getNumCapabilities
      when dbg $ printf "[SMP] initialized with capability list %s\n" 
                   (show ([0..n-1] :: [Int]))                  
      return [0..n-1]

-- | 'InitAction' for spawning threads on all capabilities, or from a
-- 'read'-able list of capability numbers in the environment variable
-- @SMP_CAPS@.
initAction :: InitAction
initAction = IA ia
  where ia sa _m = runIA (initActionForCaps getCaps) sa _m
  
-- | 'InitAction' for spawning threads only on a particular set of
-- capabilities.
initActionForCaps :: [Int] -> InitAction
initActionForCaps caps = IA ia
  where ia sa _ = do
--          setAffinityRange caps
          when dbg $ do
            printf "spawning worker threads for shared memory on caps:\n"
            printf "\t%s\n" (show caps)
          -- create a semaphore so that we only return once all the workers
          -- have been spawned
          qsem <- newQSem 0
          let caps' = nub caps
          forM_ caps' $ \n ->
            void $ spawnWorkerOnCap' qsem sa n
          forM_ caps' $ const (waitQSem qsem)
  

{-# INLINE randModN #-}
randModN :: Int -> HotVar GenIO -> IO Int
randModN caps rngRef = uniformR (0, caps-1) =<< readHotVar rngRef

-- | 'StealAction' for all capabilities.
stealAction :: Int -> StealAction
stealAction triesPerCap = SA sa
  where sa sched schedsRef = 
          runSA (stealActionForCaps getCaps triesPerCap) sched schedsRef
                 

-- | Given a set of capabilities and a number of steals to attempt per
-- capability, return a 'StealAction'.
stealActionForCaps :: [Int] -> Int -> StealAction
stealActionForCaps caps triesPerCap = SA sa
  where 
    numCaps = length caps
    numTries = numCaps * triesPerCap
    capVec = Vector.fromList caps
    sa Sched { no, rng } schedsRef = do
      scheds <- readHotVar schedsRef
      let {-# INLINE getNext #-}
          getNext :: IO Int
          getNext = randModN numCaps rng
          -- | Main steal loop
          loop :: Int -> Int -> IO (Maybe (Par ()))
          loop 0 _ = return Nothing
          loop n i | capVec Vector.! i == no = loop (n-1) =<< getNext
                   | otherwise =
            let target = capVec Vector.! i in
            case IntMap.lookup target scheds of
              Nothing -> do 
                when dbg $ 
                  printf "WARNING: no Sched for cap %d during steal\n" target
                loop (n-1) =<< getNext
              Just Sched { workpool = stealee } -> do
                mtask <- R.tryPopR stealee
                case mtask of
                  Nothing -> loop (n-1) =<< getNext
                  jtask -> return jtask                        
      loop numTries =<< getNext