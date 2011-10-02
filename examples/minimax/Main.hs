-- Time-stamp: <2009-05-06 13:54:34 simonmar>
-----------------------------------------------------------------------------

module Main where

import System.Environment
import Prog
import Board
import System.Random

main = do
    args <- getArgs
    let (version, n, depth) = case args of 
            [v, n, d] -> (v,read n, read d)
            _         -> error $ "Usage: main {nested, depth} n depth"
    setStdGen (mkStdGen 99999)
    b <- randomBoard n
    putStrLn $ showBoard b
    case version of 
        "nested" -> do 
                putStrLn "Monad-par nested version:"
                putStrLn $ solveNested depth b
        "monad"  -> do 
                putStrLn "Monad-par based version:"
                putStrLn $ solve depth b
        _        -> error$ "unknown version: "++version
