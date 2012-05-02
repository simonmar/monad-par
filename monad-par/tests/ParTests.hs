{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module ParTests (tests) where

import Control.Monad.Par.Combinator 

import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Scheds.TraceInternal (Par(..),Trace(Fork),runCont,runParAsync)

-- import Control.Monad.Par.Scheds.Direct

import Control.Concurrent.Chan
import Control.Exception 
import System.IO.Unsafe
import Data.IORef
import Test.HUnit
import Test.Framework.TH (testGroupGenerator)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
-- import Test.Framework.Providers.QuickCheck2 (testProperty)
import System.Timeout

import TestHelpers

-- -----------------------------------------------------------------------------

-- Testing

three = (3::Int)
par res m = res @=? runPar m

case_justReturn = par three (return 3)
case_oneIVar    = par three (do r <- new; put r 3; get r)


-- [2012.01.02] Apparently observing divergences here too:
case_forkNFill  = par three (do r <- new; fork (put r 3); get r)

-- [2012.05.02] The nested Trace implementation sometimes fails to
-- throw this exception, so we expect either the exception or a
-- timeout. This is reasonable since we might expect a deadlock in a
-- non-Trace scheduler. --ACF
case_getEmpty   = do
  _ <- timeout 100000 $ assertException "no result" $ 
         runPar $ do r <- new; get r
  return ()


-- [2012.01.02] Observed a blocked-indef-on-MVar failure here on
-- master branch with 16 threads:
-- 
-- | Simple diamond test.
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
disabled_case_multiput = assertException "multiple put" $ 
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

case_test_pmrr1 = 
   par 5050 $ parMapReduceRangeThresh 1 (InclusiveRange 1 100)
	        (return) (return `bincomp` (+)) 0
 where bincomp unary bin a b = unary (bin a b)


------------------------------------------------------------

-- Observe the real time ordering of events:

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



------------------------------------------------------------

tests = [ $(testGroupGenerator) ]

