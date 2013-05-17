{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module ParTests (tests) where

import Control.Monad.Par.Combinator 

-- import Control.Monad.Par.Scheds.Trace
-- import Control.Monad.Par.Scheds.TraceInternal (Par(..),Trace(Fork),runCont,runParAsync)

import Control.Monad.Par.Scheds.Direct

-- import Control.Concurrent.Chan  ()
import GHC.Conc (numCapabilities)
import Control.Exception (evaluate)
-- import System.IO.Unsafe
-- import Data.IORef
import Test.HUnit        (Assertion, (@=?))
import Test.Framework.TH (testGroupGenerator)
-- import Test.Framework (defaultMain, testGroup)
import qualified Test.Framework as TF
import           Test.Framework.Providers.HUnit 
-- import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.Timeout (timeout)

import TestHelpers (assertException, prnt, _prnt, _unsafeio, waste_time, collectOutput)

-- -----------------------------------------------------------------------------

-- Testing

three :: Int
three = 3

par :: (Eq a, Show a) => a -> Par a -> Assertion
par res m = res @=? runPar m

case_justReturn :: Assertion
case_justReturn = par three (return 3)

case_oneIVar :: Assertion
case_oneIVar    = par three (do r <- new; put r 3; get r)


-- [2012.01.02] Apparently observing divergences here too:
case_forkNFill :: Assertion
case_forkNFill  = par three (do r <- new; fork (put r 3); get r)

-- [2012.05.02] The nested Trace implementation sometimes fails to
-- throw this exception, so we expect either the exception or a
-- timeout. This is reasonable since we might expect a deadlock in a
-- non-Trace scheduler. --ACF
-- 
-- [2013.05.17] Update, it's also possible to get a blocked-indefinitely error here
--   --RRN
case_getEmpty :: IO ()
case_getEmpty   = do
  _ <- timeout 100000 $ assertException ["no result", "timeout", "thread blocked indefinitely"] $ 
         runPar $ do r <- new; get r
  return ()


-- [2012.01.02] Observed a blocked-indef-on-MVar failure here on
-- master branch with 16 threads:
-- 
-- | Simple diamond test.
case_test_diamond :: Assertion
case_test_diamond = 9 @=? (m :: Int)
 where 
  m = runPar $ do
      [a,b,c,d] <- sequence [new,new,new,new]
      fork $ do x <- get a; put b (x+1)
      fork $ do x <- get a; put c (x+2)
      fork $ do x <- get b; y <- get c; put d (x+y)
      fork $ do put a 3
      get d

-- | Violate IVar single-assignment:
--
-- NOTE: presently observing termination problems here.
-- runPar is failing to exist after the exception?
disabled_case_multiput :: IO ()
disabled_case_multiput = assertException ["multiple put"] $ 
  runPar $ do
   a <- new
   put a (3::Int)
   put a (4::Int)
   return ()


-- disabled_test3 = assertException "multiple put" $ 
--   runPar $ do
--    a <- new
--    put a (3::Int)
--    both (return 1) (return 2)
--  where 
--   -- both a b >> c  ==   both (a >> c) (b >> c)
--   -- Duplicate the continuation: is this useful for anything?
--   both :: Par a -> Par a -> Par a
--   both a b = Par $ \c -> Fork (runCont a c) (runCont b c)

case_test_pmrr1 :: Assertion
case_test_pmrr1 = 
   par 5050 $ parMapReduceRangeThresh 1 (InclusiveRange 1 100)
	        (return) (return `bincomp` (+)) 0
 where bincomp unary bin a b = unary (bin a b)


------------------------------------------------------------


-- | Observe the real time ordering of events:
--
--   Child-stealing:       
--      A D B <pause> C E
--       
--   Parent-stealing:
--      A B D <pause> C E       
--
--   Sequential:
--      A B <pause> C D E
--       
--   This is only for the TRACE scheduler right now.
--
-- This test is DISABLED because it fails unless you run with +RTS -N2
-- or greater.
--
disabled_case_async_test1 :: IO ()
disabled_case_async_test1 =
  do x <- res
     case (numCapabilities, words x) of
       (1,["A","B","C",_,"D","E"])         -> return ()       
       (n,["A","D","B","C",_,"E"]) | n > 1 -> return ()
       (n,["A","B","D","C",_,"E"]) | n > 1 -> return ()       
       _  -> error$ "Bad temporal pattern: "++ show (words x)
 where 
 res = collectOutput $ \ r -> do
  prnt r "A"
  evaluate$ runPar $
    do iv <- new
       fork $ do _prnt r "B"
                 x <- _unsafeio$ waste_time 0.5
		 _prnt r$ "C "++ show x
--		 _prnt r$ "C "++ show (_waste_time awhile)
                 put iv ()
       _prnt r "D"
       get iv
  prnt r$ "E"
  



------------------------------------------------------------

tests :: [TF.Test]
tests = [ $(testGroupGenerator) ]

