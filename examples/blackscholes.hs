{-# LANGUAGE RecordWildCards, CPP, ScopedTypeVariables, FlexibleInstances  #-}

-- Ported from CnC/C++ program by Ryan Newton
-- Then ported again from the Haskell-CnC interface to monad-par. [2011.02.16]

-- Description
-- ===========

-- The Black-Scholes equation is a differential equation that describes how,
-- under a certain set of assumptions, the value of an option changes as the 
-- price of the underlying asset changes.

-- The formula for a put option is similar. The cumulative normal distribution 
-- function, CND(x), gives the probability that normally distributed random
-- variable will have a value less than x. There is no closed form expression for
-- this function, and as such it must be evaluated numerically. The other 
-- parameters are as follows: S underlying asset's current price, 
-- X the strike price, T time to the expiration date, r risk-less rate of return,
-- and v stock's volatility.

-- Usage
-- =====

-- The command line is:

-- blackscholes b n
--     b  : positive integer for the size of blocks
--     n  : positive integer for the number of options    
-- e.g.
-- blackscholes 100000 100 4

import Control.Seq
import Control.Monad
import Control.DeepSeq
import Control.Exception

#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
#endif
import qualified Control.Monad.Par.Combinator as C
-- import Control.Monad.Par.AList

import Data.Array
import Data.List
import qualified Data.Array.Unboxed as U
import System.Environment

--------------------------------------------------------------------------------

type FpType = Float

-- This tuple contains the inputs for one invocation of our kernel
data ParameterSet =  ParameterSet {
	sptprice   :: FpType,
	strike     :: FpType,
	rate       :: FpType,
	volatility :: FpType ,
	otime      :: FpType,
	otype      :: Bool
} deriving Show

data_init :: Array Int ParameterSet

-- This defines some hard coded data as a big constant array:
#include "blackscholes_data.hs"
size_init = let (s,e) = bounds data_init in e - s + 1

inv_sqrt_2xPI = 0.39894228040143270286

--------------------------------------------------------------------------------
-- Scalar code follows:

cndf :: FpType -> FpType
cndf inputX = if sign then 1.0 - xLocal else xLocal
  where 
    sign = inputX < 0.0
    inputX' = if sign then -inputX else inputX
    
    -- Compute NPrimeX term common to both four & six decimal accuracy calcs
    xNPrimeofX = inv_sqrt_2xPI * exp(-0.5 * inputX * inputX);

    xK2 = 1.0 / (0.2316419 * inputX + 1.0);    
    xK2_2 = xK2   * xK2; -- Need all powers of xK2 from ^1 to ^5:
    xK2_3 = xK2_2 * xK2;
    xK2_4 = xK2_3 * xK2;
    xK2_5 = xK2_4 * xK2;
    
    xLocal   = 1.0 - xLocal_1 * xNPrimeofX;
    xLocal_1 = xK2   *   0.319381530  + xLocal_2;
    xLocal_2 = xK2_2 * (-0.356563782) + xLocal_3 + xLocal_3' + xLocal_3'';
    xLocal_3   = xK2_3 * 1.781477937;
    xLocal_3'  = xK2_4 * (-1.821255978);
    xLocal_3'' = xK2_5 * 1.330274429;


blkSchlsEqEuroNoDiv :: FpType -> FpType -> FpType -> FpType -> FpType -> Bool -> Float -> FpType
blkSchlsEqEuroNoDiv sptprice strike rate volatility time otype timet =
   if not otype
   then (sptprice * nofXd1) - (futureValueX * nofXd2)
   else let negNofXd1 = 1.0 - nofXd1
	    negNofXd2 = 1.0 - nofXd2
	in (futureValueX * negNofXd2) - (sptprice * negNofXd1)
 where 
   logValues  = log( sptprice / strike )                
   xPowerTerm = 0.5 * volatility * volatility
   xDen = volatility * sqrt(time)
   xD1  = (((rate + xPowerTerm) * time) + logValues) / xDen
   xD2  = xD1 -  xDen

   nofXd1 = cndf xD1 
   nofXd2 = cndf xD1    
   futureValueX = strike *  exp ( -(rate) * (time) )

--------------------------------------------------------------------------------


computeSegment :: Int -> Int -> U.UArray Int FpType
computeSegment granularity t = arr
 where 
  arr = U.listArray (0, granularity-1) $
        Prelude.map fn [0 .. granularity-1]
  fn i = let ParameterSet { .. } = data_init U.! ((t+i) `mod` size_init)
	 in blkSchlsEqEuroNoDiv sptprice strike rate volatility otime otype 0

--------------------------------------------------------------------------------

-- No need to go deeper here because its unboxed, right?
instance NFData (U.UArray Int FpType) where

main = do args <- getArgs 
          let (numOptions, granularity) =
               case args of 
  	         []      -> (10000, 1000)
  	         [b]     -> (10, read b)
	         [b,n] -> (read n, read b)

          if granularity > numOptions
	   then error "Granularity must be bigger than numOptions!!"
	   else return ()

	  putStrLn$ "Running blackscholes, numOptions "++ show numOptions ++ " and block size " ++ show granularity

          let numChunks = numOptions `quot` granularity
--	      results = runPar$ parMap (computeSegment granularity . (* granularity)) [0..numChunks-1]

#if 1
	      results = runPar$ C.parMap (computeSegment granularity) [0, granularity .. numOptions-1]
#else
-- Not working right yet [2011.02.18]
              results = toList$ runPar$ 
			parBuild 1 0 (numChunks-1) 
			  (computeSegment granularity . (* granularity))
#endif
	      sum = foldl1' (+) $ map (U.! 0) results

	  putStrLn$ "Final checksum: "++ show sum
