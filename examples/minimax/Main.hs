-- Time-stamp: <2009-05-06 13:54:34 simonmar>
-----------------------------------------------------------------------------

module Main where

import System.Environment
import Prog
import Board
import System.Random

main = do
  args <- getArgs
  let [n, depth] = case args of 
		    [n, depth] -> [read n, read depth]
		    _          -> [10,10]
		  
  setStdGen (mkStdGen 99999)
  b <- randomBoard n
  putStrLn $ showBoard b
  putStrLn $ solve depth b
