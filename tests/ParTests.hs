{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.Par
import Control.Monad.Par.Internal
import Control.Exception
import System.IO.Unsafe
import Test.HUnit


-- -----------------------------------------------------------------------------
-- both a b >> c  ==   both (a >> c) (b >> c)
-- is this useful for anything?
both :: Par a -> Par a -> Par a
both a b = Par $ \c -> Fork (runCont a c) (runCont b c)

-- -----------------------------------------------------------------------------

-- Testing

_test :: IO ()
_test = do
  print ((runPar $ return 3) :: Int)
  print (runPar $ do r <- new; put r (3 :: Int); get r)
  print (runPar $ do r <- new; fork (put r (3::Int)); get r)
  print ((runPar $ do r <- new; get r)  :: Int)

_test2 :: Int
_test2 =  runPar $ do
      [a,b,c,d] <- sequence [new,new,new,new]
      fork $ do x <- get a; put b (x+1)
      fork $ do x <- get a; put c (x+2)
      fork $ do x <- get b; y <- get c; put d (x+y)
      fork $ do put a 3
      get d

_test3 :: Int
_test3 = runPar $ do
   a <- new
   put a (3::Int)
   both (return 1) (return 2)

-- is there a standard lib thing for this?

_test_pmrr1 :: Int
_test_pmrr1 = runPar$ parMapReduceRangeThresh 1 (InclusiveRange 1 100) 
	                                      (return) (return `bincomp` (+)) 0
 where bincomp unary bin a b = unary (bin a b)

_unsafeio :: IO a -> Par a
_unsafeio io = let x = unsafePerformIO io in
	        x `seq` return x

_print :: String -> Par ()
_print = _unsafeio . putStrLn

_waste_time :: Int -> Double
_waste_time n = loop n 1.00111
  where 
    loop 0  !x             = x
    loop !n !x | x > 100.0 = loop (n-1) (x / 2)
    loop !n !x             = loop (n-1) (x + x * 0.5011)


_async_test1 = do  -- A D B <pause> C E 
  putStrLn "A"
  evaluate$ runPar $
    do 
       fork $ do _print "B"
		 _print$ "C "++ show (_waste_time 300000000)
       _print "D"
  putStrLn$ "E"

_async_test2 = do  -- A D E  or A D B E  but no C
  putStrLn "A"
  evaluate$ runParAsync $
    do 
       fork $ do _print "B"
		 _print$ "C "++ show (_waste_time 300000000)
       _print "D"
  putStrLn$ "E"


-- TODO: add the async_tests above to the test list.
_par_tests :: Test
_par_tests = 
 TestList
  [
  ]

main = runTestTT _par_tests
