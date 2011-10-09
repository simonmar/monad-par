{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Main(main) where

import Control.Monad
import Control.Monad.Par 
import Control.Monad.Par.Scheds.TraceInternal (runParAsync)
import Control.Monad.Par.Stream as S
import Control.Monad.Par.OpenList

import Control.DeepSeq
import Control.Exception

-- import Data.Array.Unboxed as U
import Data.Array.CArray as C
import Data.Complex
import Data.Int
import Data.Word
import Data.List (intersperse)
import Data.List.Split (chunk)

import System.CPUTime
import System.CPUTime.Rdtsc
import GHC.Conc as Conc
import GHC.IO (unsafePerformIO, unsafeDupablePerformIO, unsafeInterleaveIO)
import Debug.Trace

--------------------------------------------------------------------------------
-- Main script

main = do

  -- Generate 20 million elements:
  let s = countupWin 1024 (20 * 1000 * 1000) :: Par (WStream Int)
  measureRate $ runParAsync s
   
  putStrLn$ "Done with 5 million elements."
