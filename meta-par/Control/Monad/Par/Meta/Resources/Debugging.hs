{-# LANGUAGE MagicHash, UnboxedTuples, CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Monad.Par.Meta.Resources.Debugging ( dbg
                                                  , dbgTaggedMsg
                                                  , dbgDelay
                                                  , dbgCharMsg
                                                  , meaningless_alloc
                                                  , taggedmsg_global_mode
                                                  , verbosity
                                                  ) where

----------------------------------------
-- For tracing events:
-- import Foreign
import Foreign.C (CString)
import GHC.Exts (traceEvent#, Ptr(Ptr))
-- import GHC.IO hiding (liftIO)
import GHC.IO (IO(IO))
----------------------------------------

import qualified Data.ByteString.Char8 as BS
import Data.IORef             (IORef, newIORef, readIORef)
import Data.Monoid            (mappend, Monoid)
import Control.Monad          (when)
import Control.Concurrent     (myThreadId, threadDelay)
import System.IO              (hFlush, stderr)
import System.IO.Unsafe       (unsafePerformIO)
import System.Environment     (getEnvironment)


-----------------------------------------------------------------------------------
-- VERBOSITY and DEBUGGING
-----------------------------------------------------------------------------------
-- This controls how much output will be printed, 0-5.
-- 5 is "debug" mode and affects other aspects of execution (see dbgDelay)

#define TMPDBG
#define EVENTLOG

-- RRN [2012.02.28] -- Eventually for performance reasons this
-- decision will probably be made statically.  For now, in the heat of
-- debugging, it is nice to be able to change it dynamically.

-- Well, to change it truly DYNAMICALLY, it would have to be an IORef.
-- For now we just allow an environment variable to override the
-- setting at load time.

{-# NOINLINE verbosity #-}
verbosity :: Int
verbosity = unsafePerformIO$ do
--              putStrLn "GETTING ENV TO READ VERBOSITY..."
	      env <- getEnvironment
--              putStrLn$ " ENV LENGTH " ++ show (length env)
              case lookup "VERBOSITY" env of
                    -- if defined and not empty, default to 1
                    Just "" -> return 1
                    Just s  -> do let n = read s
                                  when (n >= 2)$ putStrLn "(!) Responding to VERBOSITY environment variable!"
                                  return n
#ifdef DEBUG
    	  	    Nothing -> return 5
#else
                    Nothing -> do -- putStrLn "DEFAULTING VERBOSITY TO 1" 
                                  return 1
#endif


{-# NOINLINE binaryEventLog #-}
binaryEventLog :: Bool
binaryEventLog = unsafePerformIO$ do
	      env <- getEnvironment
              case lookup "EVENTLOG" env of 
                    Nothing  -> return False
                    Just ""  -> return False                    
                    Just "0" -> return False                    
                    Just _   -> return True

-- When debugging is turned on we will do extra invariant checking:
dbg :: Bool
#ifdef DEBUG
dbg = True
#else
dbg = False
#endif

whenVerbosity :: Monad m => Int -> m () -> m ()
whenVerbosity n action = when (verbosity >= n) action

-- | dbgTaggedMsg is our routine for logging debugging output:
dbgTaggedMsg :: Int -> BS.ByteString -> IO ()
-- dbgTaggedMsg :: Int -> String -> IO ()
dbgTaggedMsg = if binaryEventLog then binaryLogMsg else textLogMsg

textLogMsg :: Int -> BS.ByteString -> IO ()
textLogMsg lvl s = 
   whenVerbosity lvl $ 
      do m   <- readIORef taggedmsg_global_mode
         tid <- myThreadId
	 BS.putStrLn$ " [distmeta" +++ m +++" "+++ sho tid +++"] "+++s

(+++) :: Monoid a => a -> a -> a
(+++) = mappend

sho :: Show a => a -> BS.ByteString
sho = BS.pack . show


binaryLogMsg :: Int -> BS.ByteString -> IO ()
binaryLogMsg lvl s = do 
--   meaningless_alloc  -- This works as well as a print for preventing the inf loop [2012.03.01]!
   whenVerbosity lvl $ do 
     m <- readIORef taggedmsg_global_mode
     tid <- myThreadId
     let msg = " [distmeta"+++m+++" "+++ sho tid+++"] "+++s
     BS.useAsCString msg myTraceEvent
     return ()

myTraceEvent :: CString -> IO ()
myTraceEvent (Ptr msg) = IO $ \s -> case traceEvent# msg s of s' -> (# s', () #)


-- | `dbgCharMsg` is for printing a small tag like '.' (with no line
--   termination) which produces a different kind of visual output.
-- dbgCharMsg :: Int -> String -> String -> IO ()
dbgCharMsg :: Int -> BS.ByteString -> BS.ByteString -> IO ()
dbgCharMsg lvl tag fullmsg = 
  if binaryEventLog 
  then dbgTaggedMsg lvl fullmsg -- It doesn't make sense to event-log a single character.
  else whenVerbosity lvl $ do BS.hPutStr stderr tag; hFlush stderr


-- When debugging it is helpful to slow down certain fast paths to a human scale:
dbgDelay :: BS.ByteString -> IO ()
dbgDelay _ = 
  if   dbg
  then threadDelay (200*1000)
  else return ()

meaningless_alloc :: IO ()
meaningless_alloc = 
   case length (fibls 5) of 
     0 -> return (error "Never happen!")
     _ -> return ()
 where 
  fibls :: Int -> [Int]
  fibls n | n <= 1 = [1::Int] 
  fibls n = fibls (n-1) ++ fibls (n-2)


{-# NOINLINE taggedmsg_global_mode #-}
-- Just for debugging, tracking global node as M (master) or S (slave):
taggedmsg_global_mode :: IORef BS.ByteString
taggedmsg_global_mode = unsafePerformIO$ newIORef "_M"
