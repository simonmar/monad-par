{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 -ddump-splices #-}
import Data.Int
import System.Environment
import GHC.Conc
import Control.Applicative
import Control.Monad.Par.Meta.Dist (longSpawn, runParDist, runParSlave, Par, get)

-- import Remote

-- necessary imports for remotable-generated code
import Control.Monad.IO.Class
import Remote2.Closure (Closure(..))
import Remote2.Encoding (Payload, serialEncodePure, serialDecode, serialEncode)
import Remote2.Reg (putReg, RemoteCallMetaData)
import Remote2.Call (mkClosure2, mkClosureRec2, remotable)

type FibType = Int64
--------------------------------------------------------------------------------

-- Par monad version + distributed execution:
parfib1 :: FibType -> Par FibType
parfib1 n | n < 2 = return 1
parfib1 n = do 
    xf <- longSpawn $ $(mkClosureRec2 'parfib1) (n-1)
    y  <-             parfib1 (n-2)
    x  <- get xf
    return (x+y)

-- Generate stub code for RPC:
remotable ['parfib1]

main = do 
    args <- getArgs
    let (version, size, cutoff) = case args of 
            []      -> ("master", 3, 1)
            [v]     -> (v,        3, 1)
            [v,n]   -> (v, read n,   1)
            [v,n,c] -> (v, read n, read c)

    case version of 
        "slave" -> runParSlave
        "master" -> do 
		       putStrLn "Using non-thresholded version:"
		       ans <- (runParDist (parfib1 size) :: IO FibType)
		       putStrLn $ "Final answer: " ++ show ans
