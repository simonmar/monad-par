{-# LANGUAGE NamedFieldPuns, CPP, BangPatterns #-}

-- | A simple interface for logging start/end times and which processor a task runs on.
--   TODO: This should be replaced by GHC events.

module Control.Monad.Par.Logging
  ( LogEntry(..), TaskSeries(..),
    allTaskSeries,
    unsafeNewTaskSeries,
    timePure,
    nameFromValue,
    grabAllLogs, printAllLogs,

    -- Timing support:
    getTime, oneSecond,
     -- TEMP:
    commaint
  )
where

import Control.Monad
import Control.DeepSeq
import Control.Exception
import Data.IORef
import Data.Word
import Data.List
import Data.List.Split (chunk)
import Data.Function
import GHC.Conc
import System.IO.Unsafe
import System.Mem.StableName
import System.CPUTime

-- Hardware cycle counter support is optional:
#define RDTSC
#ifdef RDTSC
import System.CPUTime.Rdtsc
#endif

data LogEntry = LE { start :: {-# UNPACK #-} !Word64
		   , end   :: {-# UNPACK #-} !Word64
		   , proc  :: {-# UNPACK #-} !ThreadId
		   } 
  deriving Show
type Log = [LogEntry]


-- The String identifies the task series
data TaskSeries = TS String (IORef Log)

-- Global variable that accumulates all task series.
allTaskSeries :: IORef [TaskSeries]
allTaskSeries = unsafePerformIO$ newIORef []

{-# NOINLINE unsafeNewTaskSeries #-}
unsafeNewTaskSeries :: String -> TaskSeries
unsafeNewTaskSeries name = unsafePerformIO$ 
 do log <- newIORef [] 
    let ts = TS name log
    atomicModifyIORef_ allTaskSeries (ts:)
    return ts


{-# NOINLINE timePure #-}
-- Time a pure computation, fully evaluate its result.
timePure :: NFData a => TaskSeries -> a -> a
timePure (TS _ log) thnk = unsafePerformIO$
 do proc   <- myThreadId
    start  <- rdtsc
    evaluate (rnf thnk)
    end    <- rdtsc
    atomicModifyIORef_ log (LE start end proc :)
    return thnk

{-# NOINLINE nameFromValue #-}
nameFromValue :: a -> String
nameFromValue val = unsafePerformIO$ 
  do stbl <- makeStableName val
     return ("Obj_" ++ show (hashStableName stbl))

atomicModifyIORef_ ref fn = atomicModifyIORef ref (\x -> (fn x, ()))

-- Read and reset ALL logs.
grabAllLogs :: IO [(String, Log)]
grabAllLogs = 
 do series <- readIORef allTaskSeries
    -- This is piecewise atomic.  We can't get a true snapshot but we
    -- can grab it as fast as we can:
    forM series $ \ (TS name log) -> do
      -- Atomic slice off whats there:
      ls <- atomicModifyIORef log (\x -> ([],x))
      return (name,ls)
    

printAllLogs :: IO ()
printAllLogs = 
 do grab <- grabAllLogs
    forM_ (sortBy (compare `on` fst) grab) $ \ (name, entries) -> do
      putStrLn ""
      forM_ (sortBy (compare `on` start) entries) $ \ LE{start,end,proc} -> do 
--        printf "%s %s %d %d\n" name start end (show proc)
        putStrLn$ name ++" "++ show proc ++" "++ show start ++" "++ show end

------------------------------------------------------------
-- Helpers and Scrap:

commaint :: (Show a, Integral a) => a -> String
commaint n | n < 0 = "-" ++ commaint (-n)
commaint n = 
   reverse $ concat $
   intersperse "," $ 
   chunk 3 $ reverse (show n)

------------------------------------------------------------
-- Timing support:

-- | Get the CPU time in unspecified units.
getTime :: IO Word64
-- | How many units-per-second in getTime's units.
oneSecond :: Word64

-- Having trouble with this:
#ifndef RDTSC
getTime = getCPUTime
oneSecond = 1000000000000 -- picoseconds
#else
getTime = rdtsc
oneSecond = unsafePerformIO$ measure_freq2

-- This version simply busy-waits to stay on the same core:
measure_freq2 :: IO Word64
measure_freq2 = do 
--  let second = 1000 * 1000 * 1000 * 1000 -- picoseconds are annoying
  let tenth = 100 * 1000 * 1000 * 1000 -- picoseconds are annoying      
      coef = 10
  t1 <- rdtsc 
  start <- getCPUTime
  let loop !n !last = 
       do t2 <- rdtsc 
	  when (t2 < last) $
	       putStrLn$ "WARNING, measure_freq2: COUNTERS WRAPPED "++ show (last,t2) 
	  cput <- getCPUTime		
--	  if (cput - start < second) 
	  if (cput - start < tenth)
	   then loop (n+1) t2
	   else return (n,t2)
  (n,t2) <- loop 0 t1
  putStrLn$ "  Approx getCPUTime calls per second: "++ commaint (coef * n)
  when (t2 < t1) $ 
    putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

  return$ coef * fromIntegral (t2 - t1)
#endif

