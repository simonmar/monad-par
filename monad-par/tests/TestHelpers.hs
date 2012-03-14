{-# LANGUAGE BangPatterns #-}

module TestHelpers where

import Data.List
import Prelude hiding (catch)
import Control.Exception 
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Data.Time.Clock

-- import Control.Monad.Par.Unsafe
import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Scheds.TraceInternal (Par(..),Trace(Fork),runCont,runParAsync)

------------------------------------------------------------
-- Helpers

_unsafeio :: IO a -> Par a
_unsafeio io = let x = unsafePerformIO io in
	        x `seq` return x

_waste_time :: Int -> Double
_waste_time n = loop n 1.00111
  where 
    loop 0  !x             = x
    loop !n !x | x > 100.0 = loop (n-1) (x / 2)
    loop !n !x             = loop (n-1) (x + x * 0.5011)

-- This version watches the clock so it uses a constant amount of time
-- regadless of compile/interpret mode an opt lvl.
waste_time :: Double -> IO Double
waste_time seconds = 
    do strt <- getCurrentTime
       let loop !x | x > 100.0 = chk (x / 2)
	   loop !x             = chk (x + x * 0.5011)
	   chk  !x = do t <- getCurrentTime
			if diffUTCTime t strt >= realToFrac seconds
			 then return x
			 else loop x
       loop  1.00111

-- Obviously this takes a lot longer if it's interpreted:
--awhile = 300000000
awhile = 3 * 1000 * 1000
-- awhile = 300000

atomicModifyIORef_ rf fn = atomicModifyIORef rf (\x -> (fn x, ()))


-- Haskell doesn't offer a way to create a Handle for in-memory output.
-- So here we use IORefs instead
collectOutput :: (IORef [String] -> IO ()) -> IO String
collectOutput fn = 
  do c <- newIORef []
     fn c
     ls <- readIORef c
     return (unlines (reverse ls))

prnt :: IORef [String] -> String -> IO ()
prnt ref str = atomicModifyIORef_ ref (str:)

_prnt :: IORef [String] -> String -> Par ()
_prnt ref = _unsafeio . prnt ref
     


-- -----------------------------------------------------------------------------

-- assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
-- assertException ex action =
--     handleJust isWanted (const $ return ()) $ do
--         action
--         assertFailure $ "Expected exception: " ++ show ex
--   where isWanted = guard . (== ex)

-- Ensure that evaluating an expression returns an exception
assertException  :: String -> a -> IO ()
assertException msg val = do
 x <- catch (do evaluate val; return Nothing) 
            (\e -> do putStrLn$ "Good.  Caught exception: " ++ show (e :: SomeException)
                      return (Just$ show e))
 case x of 
  Nothing -> error "Failed to get an exception!"
  Just s -> 
   if isInfixOf msg s 
   then return () 
   else error$ "Got the wrong exception, expected to see the text: "++ show msg 
	       ++ "\nInstead got this exception:\n  " ++ show s
     
