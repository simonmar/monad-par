{-# LANGUAGE BangPatterns, ScopedTypeVariables, CPP #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

-- The goal of this benchmark is to demonstrate kernels that have private
-- state which has to be read from memory on each invocation.  Further,
-- the state forms the majority of the working set -- it's larger than
-- the streaming input.

-- In theory this means that keeping a kernel on one core and
-- exploiting pipeline parallelism is better than following the data
-- through the stream graph in a depth first traversal.


--module Main(main) where
module Main where

import Control.Monad as C
import Control.DeepSeq
import Control.Exception
import qualified Control.Parallel.Strategies as Strat
-- import Data.Array.Unboxed as U
import Data.Int
import Prelude as P
import System.Environment
import System.Exit
import GHC.Conc as Conc
-- import GHC.IO (unsafePerformIO, unsafeDupablePerformIO, unsafeInterleaveIO)

import Debug.Trace
import Control.Monad.Par.Logging

import qualified Data.Vector.Unboxed as UV
import           Data.Vector.Unboxed hiding ((++))

-- TEMP: Currently [2011.10.20] the Stream module hasn't been
-- generalized over the ParIVar type class and thus this example
-- currently works only with the Trace scheduler:
import Control.Monad.Par.Stream as S
import Control.Monad.Par.Scheds.Trace

-- #ifdef PARSCHED 
-- import PARSCHED
-- #else
-- import Control.Monad.Par
-- #endif



-- Performs some (presently meaningless) computation on a state & a
-- window (stream element) to compute a new state and new window.
--
-- Assumes statesize is a multiple of bufsize:
statefulKern :: Vector Double -> Vector Double -> (Vector Double, Vector Double)
statefulKern state buf = (newstate, newelem)
 where 
  -- We could probably test the memory behavior we're interested in
  -- better with inplace update here... but for now, this:
  newstate = UV.map (\d -> d/sum + 2) state
  newelem  = UV.map (+sum) buf

  sum = P.sum partialSums
  partialSums = [ sumslice (cutslice n) | n <- [0..factor-1] ]   

  cutslice n    = UV.slice (n*blen) blen state
  sumslice slce = UV.sum (UV.zipWith (+) buf slce)

  factor       = slen `quot` blen
  slen = UV.length state
  blen = UV.length buf

--------------------------------------------------------------------------------

monadpar_version (_,numfilters, bufsize, statecoef, numwins) = do 
  putStrLn$ "Running monad-par version."

  let statesize = bufsize * statecoef
  results <- evaluate $ runPar$ do 
       strm1 :: Stream (UV.Vector Double) <- S.generate numwins (\n -> UV.replicate bufsize 0)
       -- Make a pipeline of numfilters stages:
       let initstate = UV.generate statesize fromIntegral
       pipe_end <- C.foldM (\s _ -> streamScan statefulKern initstate s) strm1 [1..numfilters]

       sums  <- streamMap UV.sum pipe_end
#if 0
       return sums

  -- This is tricky, but two different consumers shouldn't prevent
  -- garbage collection.
  ls <- toListSpin results
--  Just (Cons h _) <- pollIVar results
  putStrLn$ "Sum of first window: " ++ show (P.head ls)
  forkIO$ measureRateList ls
  putStrLn$ "Final sum = "++ show (P.sum ls)
#else

       streamFold (+) 0 sums

  putStrLn$ "Final sum = "++ show results
#endif


--------------------------------------------------------------------------------

sparks_version (_,numfilters, bufsize, statecoef, numwins) = do 
  putStrLn$ "Running sparks version."

  -- Here we represent the stream as a plain list.
  let 
      statesize = bufsize * statecoef
      strm1 :: [UV.Vector Double] = P.replicate numwins $ UV.replicate bufsize 0
      initstate = UV.generate statesize fromIntegral
      applyKern = scan statefulKern initstate

      -- This one has the problem that it fully evaluates the stream for the
      -- first kernel before moving on to the second:
      --      strm_last = (parRepeatFun numfilters applyKern) strm1

      pipe_end = applyNKernels statefulKern numfilters initstate strm1

      sums = P.map UV.sum pipe_end
-- #define SERIAL
#ifndef SERIAL
	     `Strat.using` (Strat.parBuffer numCapabilities Strat.rseq) 
#endif

  putStrLn$ "Sum of first window: "++ show (P.head sums)
  measureRateList (sums)
--  measureRateList (strm_last)
--  measureRateList (forceList strm_last)
  putStrLn$ "Final Sum = " ++ show (P.sum sums)

-- Make sure the cars of a list are evaluated before following each cdr:
forceList [] = []
forceList (h:t) = rnf h `seq` forceList t

-- A slightly different version of Data.List.scanl
scan :: (a -> b -> (a,c)) -> a -> [b] -> [c]
scan f q [] = []
scan f q (h:t) = h' : scan f q t
 where 
  (q',h') = f q h

type StatefulKernel s a b = s -> a -> (s,b)

-- applyNKernels _ _ _ [] = []
-- applyNKernels :: NFData a => StatefulKernel s a a -> Int -> s -> [a] -> [a]
applyNKernels :: (NFData a, NFData s) => StatefulKernel s a a -> Int -> s -> [a] -> [a]
applyNKernels _    0 _    ls = ls
applyNKernels kern n init ls =   
  applyNKernels kern (n-1) init (loop init ls)
 where 
  tasklog = unsafeNewTaskSeries (nameFromValue (n,kern))

  loop st [] = []
  loop st (h:t) = 
    let (st', x) = 
#if 0
	           timePure tasklog$ 
#endif
		   kern st h in
#ifndef SERIAL
    rnf x `par` 
#endif
     x : loop st' t
   
-- Compose two stateful kernels in parallel.
composeStatefulKernels :: (NFData b, NFData s1) => 
			  StatefulKernel s1 a b -> StatefulKernel s2 b c 
		       -> StatefulKernel (s1,s2) a c
-- composeStatefulKernels (f1,f2) (s1,s2) x = 
composeStatefulKernels f1 f2 (s1,s2) x = 
    rnf pr1 `par` (newstate, snd pr2)
 where 
  pr1 = f1 s1 x
  pr2 = f2 s2 (snd pr1)
  newstate = (fst pr1, fst pr2)


parRepeatFun n f = 
--  P.foldr (.) id (P.replicate n f)
  P.foldr (Strat..|| Strat.rdeepseq) id (P.replicate n f)


--------------------------------------------------------------------------------
-- Main script

default_version = "monad"
default_numfilters = 4
default_bufsize    = 64
default_statecoef  = 8   -- in MULTIPLES of bufsize
default_numwins    = 500
-- 4 256 10 10000

main = do
  args <- getArgs
  arg_tup@(version,_,_,_,_) <- 
       case args of 
	 []          -> return (default_version, default_numfilters, default_bufsize, default_statecoef, default_numwins)
	 [ver]       -> return (            ver, default_numfilters, default_bufsize, default_statecoef, default_numwins)
	 [a,b,c,d,e] -> return (a, read b, read c, read d, read e)
	 _         -> do 
	               putStrLn$ "ERROR: Invalid arguments, must take 0,1, or 5 args."
		       putStrLn$ "  Expected args: (version='monad'|'sparks' #filters, bufsize, stateSizeMultiplier, #bufsToProcess)"
		       putStrLn$ "  Received args: "++ show args
		       exitFailure 

  putStrLn$ "numCapabilities: "++ show numCapabilities
  putStrLn$ "  Frequency in measurable ticks:  "++ commaint oneSecond ++ "\n"

  case version of 
    "monad"  -> monadpar_version arg_tup
    "sparks" -> sparks_version  arg_tup
    _        -> error$ "unknown version: "++version

-- This is very verbose:
--  putStrLn$ "Finally, dumping all logs:"
--  printAllLogs



-- It is not necessary to evaluate every element in the case of an unboxed vector.
instance NFData a => NFData (UV.Vector a) where
 rnf !vec = ()


print_ msg = trace msg $ return ()

-- work pop 1 peek N push 1 
-- float->float filter 
-- firFilter n coefs = 
-- {

--     float sum = 0;
--     for (int i = 0; i < N; i++)
--       sum += peek(i) * COEFF[N-1-i];
--     pop();
--     push(sum);
--   }
-- }


{- 

Here's what cachegrind says (on 4 core nehalem):

  $ valgrind --tool=cachegrind ./stream/disjoint_working_sets_pipeline monad 4 768 10 1000 +RTS -N4
   .....
      [measureRate] current rate: 58  Total elems&time 916  181,988,055,721
      [measureRate] Hit end of stream after 1000 elements.
     Final sum = 1.560518243231086e22
     ==21202== 
     ==21202== I   refs:      7,111,462,273
     ==21202== I1  misses:          374,190
     ==21202== L2i misses:          298,364
     ==21202== I1  miss rate:          0.00%
     ==21202== L2i miss rate:          0.00%
     ==21202== 
     ==21202== D   refs:      3,882,935,974  (3,542,949,529 rd   + 339,986,445 wr)
     ==21202== D1  misses:       14,606,684  (    9,824,455 rd   +   4,782,229 wr)
     ==21202== L2d misses:        6,774,479  (    2,088,565 rd   +   4,685,914 wr)
     ==21202== D1  miss rate:           0.3% (          0.2%     +         1.4%  )
     ==21202== L2d miss rate:           0.1% (          0.0%     +         1.3%  )
     ==21202== 
     ==21202== L2 refs:          14,980,874  (   10,198,645 rd   +   4,782,229 wr)
     ==21202== L2 misses:         7,072,843  (    2,386,929 rd   +   4,685,914 wr)
     ==21202== L2 miss rate:            0.0% (          0.0%     +         1.3%  )


Sparks version:
     Final Sum = 1.560518243231086e22
     ==21226== 
     ==21226== I   refs:      5,898,314,238
     ==21226== I1  misses:          291,271
     ==21226== L2i misses:          246,518
     ==21226== I1  miss rate:          0.00%
     ==21226== L2i miss rate:          0.00%
     ==21226== 
     ==21226== D   refs:      3,264,359,909  (3,206,394,437 rd   + 57,965,472 wr)
     ==21226== D1  misses:       16,003,068  (   10,905,138 rd   +  5,097,930 wr)
     ==21226== L2d misses:        9,177,043  (    4,207,106 rd   +  4,969,937 wr)
     ==21226== D1  miss rate:           0.4% (          0.3%     +        8.7%  )
     ==21226== L2d miss rate:           0.2% (          0.1%     +        8.5%  )
     ==21226== 
     ==21226== L2 refs:          16,294,339  (   11,196,409 rd   +  5,097,930 wr)
     ==21226== L2 misses:         9,423,561  (    4,453,624 rd   +  4,969,937 wr)
     ==21226== L2 miss rate:            0.1% (          0.0%     +        8.5%  )

 -}
