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
import Control.Monad.Par 
import Control.Monad.Par.Stream as S
import Control.Monad.Par.OpenList
import Control.DeepSeq
import Control.Exception
import Control.Parallel.Strategies as Strat

-- import Data.Array.Unboxed as U
import Data.Complex
import Data.Int
import Data.Word
import Data.List  (intersperse) 
import Data.List.Split (chunk)

import Prelude as P
import System.Environment
import System.Exit
import System.CPUTime
import System.CPUTime.Rdtsc
import GHC.Conc as Conc
import GHC.IO (unsafePerformIO, unsafeDupablePerformIO, unsafeInterleaveIO)
import Debug.Trace

import qualified Data.Vector.Unboxed as UV
import           Data.Vector.Unboxed hiding ((++))

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
  results <- evaluate $ runParAsync$ do 
       strm1 :: Stream (UV.Vector Double) <- S.generate numwins (\n -> UV.replicate bufsize 0)
       -- Make a pipeline of numfilters stages:
       let initstate = UV.generate statesize fromIntegral
       pipe_end <- C.foldM (\s _ -> streamScan statefulKern initstate s) strm1 [1..numfilters]

       sums  <- streamMap UV.sum pipe_end

       return sums
--       summed <- streamMap UV.sum strm_last
--       return summed

  -- This is tricky, but two different consumers shouldn't prevent
  -- garbage collection.
  ls <- toListSpin results

--  Just (Cons h _) <- pollIVar results
  putStrLn$ "Sum of first window: " ++ show (P.head ls)
  forkIO$ measureRateList ls
  putStrLn$ "Final sum = "++ show (P.sum ls)

--  browseStream results


  

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
	     `using` (Strat.parBuffer numCapabilities rwhnf) 
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
applyNKernels :: NFData a => StatefulKernel s a a -> Int -> s -> [a] -> [a]
applyNKernels _    0 _    ls = ls
applyNKernels kern n init ls =   
  applyNKernels kern (n-1) init (loop init ls)
 where 
  loop st [] = []
  loop st (h:t) = 
    let (st', x) = kern st h in
#ifndef SERIAL
    rnf x `par` 
#endif
     x : loop st' t
   
-- Compose two stateful kernels in parallel.
#if 1
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
#endif

-- This version forces both to have the same state type...  could do
-- something more sophisticated here.
-- composeStatefulKernels :: (NFData b, NFData s1) => 
-- 			  StatefulKernel [s] a b -> StatefulKernel [s] b c 
-- 		       -> StatefulKernel [s] a c



parRepeatFun n f = 
--  P.foldr (.) id (P.replicate n f)
  P.foldr (.|| rdeepseq) id (P.replicate n f)

-- foo a b c = a .|| b  c

--------------------------------------------------------------------------------
-- Main script

default_version = "monad"
default_numfilters = 4
default_bufsize    = 256
default_statecoef  = 10   -- in MULTIPLES of bufsize
default_numwins    = 10 * 1000


main = do
  args <- getArgs
  arg_tup@(version,_,_,_,_) <- 
       case args of 
	 []          -> return (default_version, default_numfilters, default_bufsize, default_statecoef, default_numwins)
	 [a,b,c,d,e] -> return (a, read b, read c, read d, read e)
	 _         -> do 
	               putStrLn$ "ERROR: Invalid arguments, must take 0 or 5 args."
		       putStrLn$ "  Expected args: (version='monad'|'sparks' #filters, bufsize, stateSizeMultiplier, #bufsToProcess)"
		       putStrLn$ "  Received args: "++ show args
		       exitFailure 

  putStrLn$ "numCapabilities: "++ show numCapabilities
  putStrLn$ "  Frequency in measurable ticks:  "++ commaint one_second ++ "\n"

  case version of 
    "monad"  -> monadpar_version arg_tup
    "sparks" -> sparks_version  arg_tup
    _        -> error$ "unknown version: "++version



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


