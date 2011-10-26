{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module ParTests where

import Control.Monad.Par.Combinator 
import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Scheds.TraceInternal
import Control.Exception 
import Control.Concurrent.Chan
import System.IO.Unsafe
import Test.HUnit

import Data.List
import Data.IORef
import Data.Time.Clock
import Prelude hiding (catch)
import Test.Framework.TH (testGroupGenerator)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2 (testProperty)

-- -----------------------------------------------------------------------------
-- Scrap: 

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
     
-- -----------------------------------------------------------------------------

-- Testing

three = (3::Int)
par res m = res @=? runPar m

case_justReturn = par three (return 3)
case_oneIVar    = par three (do r <- new; put r 3; get r)
case_forkNFill  = par three (do r <- new; fork (put r 3); get r)

-- This is an error:
case_getEmpty   = assertException "no result" $ 
		  ((runPar $ do r <- new; get r) :: Int)

case_test2 = 9 @=? (m :: Int)
 where 
  m = runPar $ do
      [a,b,c,d] <- sequence [new,new,new,new]
      fork $ do x <- get a; put b (x+1)
      fork $ do x <- get a; put c (x+2)
      fork $ do x <- get b; y <- get c; put d (x+y)
      fork $ do put a 3
      get d

-- Multiple puts:  How can we catch this exception??
disabled_test3 = assertException "multiple put" $ 
  runPar $ do
   a <- new
   put a (3::Int)
   both (return 1) (return 2)
 where 
  -- both a b >> c  ==   both (a >> c) (b >> c)
  -- is this useful for anything?
  both :: Par a -> Par a -> Par a
  both a b = Par $ \c -> Fork (runCont a c) (runCont b c)

case_test_pmrr1 = 
   par 5050 $ parMapReduceRangeThresh 1 (InclusiveRange 1 100)
	        (return) (return `bincomp` (+)) 0
 where bincomp unary bin a b = unary (bin a b)


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
     
------------------------------------------------------------

-- A D B <pause> C E 
case_async_test1 = 
  do x <- res
     case words x of 
       ["A","D","B","C",_,"E"] -> return ()
       _  -> error$ "Bad output: "++ show (words x)
 where 
 res = collectOutput $ \ r -> do
  prnt r "A"
  evaluate$ runPar $
    do 
       fork $ do _prnt r "B"
                 x <- _unsafeio$ waste_time 0.5
		 _prnt r$ "C "++ show x
--		 _prnt r$ "C "++ show (_waste_time awhile)
       _prnt r "D"
  prnt r$ "E"

-- A D E  or A D B E  but no C
--
-- ODD -- this passes when run manually (compiled or interpreted) but
-- fails when run through the test framework [2011.10.25].
case_async_test2 = 
  do x <- async2
     case words x of 
       ["A","D","E"]     -> return ()
       ["A","D","B","E"] -> return ()
       _  -> error$ "async_test2: Bad output: "++ show (words x)

async2 = collectOutput $ \ r -> do 
  prnt r "A"
  evaluate$ runParAsync $
    do 
       fork $ do _prnt r "B"
		 x <- _unsafeio$ waste_time 0.5
		 _prnt r$ "C "++ show x
--		 _prnt r$ "C "++ show (_waste_time awhile)
       _prnt r "D"
  prnt r "E"


------------------------------------------------------------

tests = [ $(testGroupGenerator) ]

-- It works fine when run like this:
main = do x <- async2; putStrLn x
	  case_async_test2
--	  case_async_test1
