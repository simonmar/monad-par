{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
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

-- import Data.Array.Unboxed as U
import Data.Complex
import Data.Int
import Data.Word
import Data.List (intersperse)
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
-- Main script

default_numfilters = 4
default_bufsize    = 256
default_statecoef  = 10   -- in MULTIPLES of bufsize
default_numwins    = 10 * 1000

main = do
  args <- getArgs
  (numfilters, bufsize, statecoef, numwins) <- 
       case args of 
	 []        -> return (default_numfilters, default_bufsize, default_statecoef, default_numwins)
	 [a,b,c,d] -> return (read a, read b, read c, read d)
	 _         -> do 
	               putStrLn$ "ERROR: Invalid arguments, must take 0 or 4 args."
		       putStrLn$ "  Expected args: (#filters, bufsize, stateSizeMultiplier, #bufsToProcess)"
		       putStrLn$ "  Received args: "++ show args
		       exitFailure 

  putStrLn$ "numCapabilities: "++ show numCapabilities
  putStrLn$ "  Frequency in measurable ticks:  "++ commaint one_second ++ "\n"

  let statesize = bufsize * statecoef
  results <- evaluate $ runParAsync$ do 

       strm1 :: Stream (UV.Vector Double) <- S.generate numwins (\n -> UV.replicate bufsize 0)

       print_$ "\n Next, applying filter... "

       -- Make a pipeline of N stages:
       let initstate = UV.generate statesize fromIntegral
       strm_last <- C.foldM (\s _ -> streamScan statefulKern initstate s) strm1 [1..numfilters]

       return strm_last

  measureRate results
--  browseStream results
  putStrLn$ "Done"


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


