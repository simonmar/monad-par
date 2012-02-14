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
import Remote2.Call (mkClosure2)

type FibType = Int64

-- IO version of fib for remotable TH testing
fibIO :: FibType -> IO FibType
fibIO 0 = return 1
fibIO 1 = return 1
fibIO x = (+) <$> fibIO (x-2) <*> fibIO (x-1)

-- ( remotable ['fibIO] )


fibPar :: FibType -> Par FibType
fibPar 0 = return 1
fibPar 1 = return 1
fibPar x = (+) <$> fibPar (x-2) <*> fibPar (x-1)

--------------------------------------------------------------------------------
-- Hand-tweaked Closure and RemoteCallMetaData code

parfib1__0__impl :: Payload -> IO FibType
parfib1__0__impl a
  = do res <- liftIO (serialDecode a)
       case res of 
         Just a1 -> liftIO $ runParDist (parfib1 a1)
         _ -> error "Bad decoding in closure splice of parfib1"

parfib1__0__implPl :: Payload -> IO Payload
parfib1__0__implPl a
  = do res <- parfib1__0__impl a
       liftIO (serialEncode res)

parfib1__closure :: FibType -> Remote2.Closure.Closure (Par FibType)
parfib1__closure
  = \ a1
      -> Closure
           "Main.parfib1__0__impl" (serialEncodePure a1)

__remoteCallMetaData :: RemoteCallMetaData
__remoteCallMetaData x
  = putReg
      parfib1__0__impl
      "Main.parfib1__0__impl"
      (putReg
         parfib1__0__implPl
         "Main.parfib1__0__implPl"
         (putReg parfib1__closure "Main.parfib1__closure" x))

--------------------------------------------------------------------------------

-- Par monad version:
parfib1 :: FibType -> Par FibType
parfib1 n | n < 2 = return 1
parfib1 n = do 
--    xf <- longSpawn $ parfib1__closure (n-1)
    xf <- longSpawn $ $(mkClosure2 'parfib1) (n-1)
    y  <-             parfib1 (n-2)
    x  <- get xf
    return (x+y)

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
		       ans <- (runParDist 
			         -- (Just "config") 
--			       [__remoteCallMetaData]
			       (parfib1 size) :: IO FibType)
		       putStrLn $ "Final answer: " ++ show ans
