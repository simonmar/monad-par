#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

-- A simple script to file away results_XYZ.dat and bench_XYZ.log in their respective places.

-- The way to use this script is after you run benchmark.hs you run 
--     ./file_away.hs HOST ../results/MyMachineDescription/

module Main where


import Control.Monad

import Data.List
import Data.Char
import Data.List.Split
import Data.Time.Clock
import Data.Time.Calendar

import HSH
import System.Environment
import System.Directory
import System.FilePath 
 
import Text.Printf

-- import Text.PrettyPrint.HughesPJClass
-- import Text.Regex
-- import Data.List
-- import Data.Maybe (mapMaybe)
-- import Data.Function
-- import Control.Monad
-- import System.Process (system)
-- import System.IO
-- import System.Environment

date :: IO (Integer,Int,Int) -- :: (year,month,day)
date = getCurrentTime >>= return . toGregorian . utctDay

main = do 
  args <- getArgs
  (host,root) <- case args of 
	          [name, root] -> return (name,root)
	          ls -> do putStrLn "Usage file_away <HOST> <results_dir_for_machine>"
			   putStrLn "  This script will file away ./results_HOST.dat and ./bench_HOST.log"
                           error$ " Incorrect arguments,  " ++ show (length args) ++" args: "++ unwords args
  -- This is insufficient to get the directory name:
  -- prg <- getProgName

  when (elem '/' host) $ error$ "HOST argument should be a simple word, not:\n  "++host

  -- Source files:
  let rf = "results_" ++ host ++ ".dat"                        
      bf = "bench_"   ++ host ++ ".log"
  r <- doesFileExist rf
  b <- doesFileExist bf 

  unless r$ error$ "File "++ rf ++ " does not exist!"
  unless b$ error$ "File "++ bf ++ " does not exist!"

  -- Goal:
  -- results_basalt_2011_10_15_desktop_v1.dat  
  (y,m,d) <- date

  variantL <- runSL $ catFrom [rf] -|- grep "Benchmarks_Variant"
  versionL <- runSL $ catFrom [rf] -|- grep "Benchmarks_Version"
  ghcVerL  <- runSL $ catFrom [rf] -|- grep "Glorious Glasgow"

  let strip = trim . (filter (/='#'))
      -- Here we have some strong expectations about the textual format of the file:
      [_,variant] = words$ strip variantL
      [_,version] = words$ strip versionL 
      rdname::String = printf "results_%s_%d_%d_%d_%s_v%s.dat" host y m d variant version
      bdname::String = printf "bench_%s_%d_%d_%d_%s_v%s.log"   host y m d variant version
      
      ghcVer = head $ reverse $ words ghcVerL
      ghcDir = "ghc-"++ghcVer
      absDir = root </> ghcDir

  e <- doesDirectoryExist absDir
  unless e $ do printf "Creating directory: %s\n" absDir
		createDirectory absDir

  printf "Moving %s -> %s/%s \n" rf absDir rdname
  renameFile rf (absDir </> rdname)

  printf "Moving %s -> %s/%s \n" bf absDir bdname
  renameFile bf (absDir </> bdname)
  
  return ()



-- Remove whitespace from both ends of a string:
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
