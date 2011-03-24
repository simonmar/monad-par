{-# LANGUAGE NamedFieldPuns #-}
-- A simple interface for logging start/end times and which processor a task runs on.

module Control.Monad.Par.Logging
  ( LogEntry(..), TaskSeries(..),
    allTaskSeries,
    unsafeNewTaskSeries,
    timePure,
    nameFromValue,
    grabAllLogs, printAllLogs
  )
where

import Control.Monad
import Control.DeepSeq
import Control.Exception
import Data.IORef
import Data.Word
import Data.List
import Data.Function
import GHC.Conc
import System.CPUTime.Rdtsc
import System.IO.Unsafe
import System.Mem.StableName

-- import Text.Printf

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