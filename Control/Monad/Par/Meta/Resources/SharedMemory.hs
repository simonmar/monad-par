{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-do-bind #-}

module Control.Monad.Par.Meta.Resources.SharedMemory (
    initAction
  , stealAction
  ) where

import Control.Concurrent
import Control.Monad

import Data.Concurrent.Deque.Reference as R
import qualified Data.IntMap as IntMap
import Data.List

import System.Random    

import Text.Printf

import Control.Monad.Par.Meta hiding (dbg, stealAction)
import Control.Monad.Par.Meta.HotVar.IORef

dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

-- | 'InitAction' for spawning threads on all capabilities.
initAction :: InitAction
initAction sa _m = do
  caps <- getNumCapabilities
  initActionForCaps [0..caps-1] sa _m
  
-- | 'InitAction' for spawning threads only on a particular set of
-- capabilities.
initActionForCaps :: [Int] -> InitAction
initActionForCaps caps sa _ = do
  when dbg $ do
    printf "spawning worker threads for shared memory on caps:\n"
    printf "\t%s\n" (show caps)
  (cap, _) <- threadCapability =<< myThreadId
  forM_ (nub caps) $ \n ->
    when (n /= cap) $ void $ spawnWorkerOnCap sa n       

randModN :: Int -> HotVar StdGen -> IO Int
randModN caps rngRef = 
  modifyHotVar rngRef $ \g ->
      let (n, g') = next g
          i = n `mod` caps
      in (g', i)

-- | 'StealAction' for all capabilities.
stealAction :: Int -> StealAction
stealAction triesPerCap sched schedsRef = do
  caps <- getNumCapabilities
  stealActionForCaps [0..caps-1] triesPerCap sched schedsRef

-- | Given a set of capabilities and a number of steals to attempt per
-- capability, return a 'StealAction'.
stealActionForCaps :: [Int] -> Int -> StealAction
stealActionForCaps caps triesPerCap Sched { no, rng } schedsRef = do
  scheds <- readHotVar schedsRef
  let numCaps = length caps
      getNext :: IO Int
      getNext = randModN numCaps rng
      numTries = numCaps * triesPerCap
      -- | Main steal loop
      loop :: Int -> Int -> IO (Maybe (Par ()))
      loop 0 _ = return Nothing      
      loop n i | caps !! i == no = loop (n-1) =<< getNext
               | otherwise =
        let target = (caps !! i) in
        case IntMap.lookup target scheds of
          Nothing -> do 
            void $ printf "WARNING: no Sched for cap %d during steal\n" target
            loop (n-1) =<< getNext
          Just Sched { workpool = stealee } -> do
            mtask <- R.tryPopR stealee
            case mtask of
              Nothing -> loop (n-1) =<< getNext
              jtask -> return jtask                        
  loop numTries =<< getNext