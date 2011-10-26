#!/usr/bin/env runhaskell

-- This script goes over all the .summary files to produce a summary
-- of summaries across different machines.
-- 
-- Run "plot_ALL.hs" before running this script.

module Main where

import Control.Monad
import HSH
import ScriptHelpers

data Entry = Entry { 
  name     :: String,
  variant  :: String,
  threads  :: Int, 
  time     :: Double,
  speedup  :: Double
}

-- TODO: Update plot_scaling.hs to dig around a bit more in the .dat
-- files and assemble a proper one-word representation of the
-- benchmark arguments.

-- Alternatively, partition the following by benchlist version (+ server|desktop)

parse [name,-,sched,threads,time,speedup] =
   undefined
--  Entry {name,variant,}


main = do 
  files <- run "ls */*/results_*.summary"

  putStrLn "UNFINISHED SCRIPT"
  
  forM_ files $ \file -> do
     putStrLn file
     dat <- readDataFile file
     mapM_ print (map parse dat)
     putStrLn ""

