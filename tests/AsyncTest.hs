{-# LANGUAGE TemplateHaskell #-}

module AsyncTest (tests, manual_main) where

import Control.Exception 
import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Scheds.TraceInternal (Par(..),Trace(Fork),runCont,runParAsync)
import Test.Framework.TH (testGroupGenerator)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import TestHelpers

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

tests = [ $(testGroupGenerator) ]

-- It works fine when run like this:
manual_main = do x <- async2; putStrLn x
                 case_async_test2
--	  case_async_test1
