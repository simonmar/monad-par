{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Resources.SharedMemory (
    initAction
  , stealAction
  ) where

import Control.Concurrent
import Control.Monad

import Data.Concurrent.Deque.Reference as R

-- import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

-- import GHC.Conc

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

-- | 'Int' argument controls the number of steal attemps to make per
-- capability on a given execution of the 'StealAction'.
initAction :: Int -> InitAction
initAction triesPerCap _ = do
  when dbg $ printf "spawning worker threads for shared memory\n"
  caps <- getNumCapabilities
  (cap, _) <- threadCapability =<< myThreadId
  forM_ [0..caps-1] $ \n ->
    when (n /= cap) $ void $ spawnWorkerOnCap (stealAction caps triesPerCap) n

randModN :: Int -> HotVar StdGen -> IO Int
randModN caps rngRef = 
  modifyHotVar rngRef $ \g ->
      let (n, g') = next g
          i = n `mod` caps
      in (g', i)

-- | Given the number of capabilities at initialization and a number
-- of steals to attempt per capability, return a 'StealAction'.
stealAction :: Int -> Int -> StealAction
stealAction caps triesPerCap Sched { no, rng } schedsRef = do
  scheds <- readHotVar schedsRef
  let getNext :: IO Int
      getNext = randModN caps rng
      numTries = caps * triesPerCap
      -- | Main steal loop
      loop :: Int -> Int -> IO (Maybe (Par ()))
      loop 0 _ = return Nothing      
      loop n i | i == no   = loop (n-1) =<< getNext
               | otherwise = 
        case IntMap.lookup i scheds of
          Nothing -> do 
            void $ printf "WARNING: no Sched for cap %d during steal\n" i
            loop (n-1) =<< getNext
          Just Sched { workpool = stealee } -> do
            mtask <- R.tryPopR stealee
            case mtask of
              Nothing -> loop (n-1) =<< getNext
              jtask -> return jtask
                        
  loop numTries =<< getNext