{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 -ddump-splices #-}
import Data.Int (Int64)
import System.Environment (getArgs)
import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Par.Meta.Dist 
import Control.Concurrent   (myThreadId)
import System.Process       (readProcess)
import System.Posix.Process (getProcessID)
import Data.Char            (isSpace)

import Control.Monad.Par.Meta.DistSMP
        (longSpawn, Par, get, shutdownDist, WhichTransport(Pipes,TCP),
	 runParDistWithTransport, runParSlaveWithTransport, spawn_)
-- Tweaked version of CloudHaskell's closures:
import RPC.Call (mkClosureRec, remotable)
-- import DistDefaultMain

import Data.Time.Clock
import GHC.Conc
import Control.Concurrent.MVar
import Control.Monad.Trans (liftIO)


type FibType = Int64

--------------------------------------------------------------------------------

-- Par monad version + distributed execution:
-- This version is NOT thresholded.
parfib0 :: Int -> Par FibType
parfib0 n | n < 2     = return 1
	  | otherwise = do
    xf <- longSpawn$ $(mkClosureRec 'parfib0) (n-1)
    y  <-             parfib0                 (n-2)
    x  <- get xf
    return (x+y)

--------------------------------------------------------------------------------

-- Par monad version + distributed execution:
parfib1 :: (Int,Int,Int) -> Par FibType
parfib1 (n,thresh1,thresh2) | n < 2        = return 1
		            | n <= thresh1 = parfib2 (n,thresh2)
		            | otherwise    = do
#if 0
    liftIO $ do 
       mypid <- getProcessID
       mytid <- myThreadId
       host  <- hostName
       putStrLn $ " [host "++host++" pid "++show mypid++" "++show mytid++"] PARFIB "++show n
#endif
    xf <- longSpawn$ $(mkClosureRec 'parfib1) (n-1, thresh1,thresh2)
    y  <-             parfib1                 (n-2, thresh1,thresh2)
    x  <- get xf
    return (x+y)

parfib2 :: (Int,Int) -> Par FibType
parfib2 (n,thresh) = loop n
 where
  loop n | n < 2      = return 1
  loop n | n < thresh = parfib3 n
  loop n = do 
    xf <- spawn_$ loop (n-1)
    y  <-         loop (n-2)
    x  <- get xf
    return (x+y)


parfib3 :: Int -> Par FibType
parfib3 n | n < 2 = return 1
parfib3 n = do 
    x <- parfib3 (n-1)
    y <- parfib3 (n-2)
    return (x+y)


------------------------------------------------------------

hostName = do s <- readProcess "hostname" [] ""
	      return (trim s)
 where 
  -- | Trim whitespace from both ends of a string.
  trim :: String -> String
  trim = f . f
     where f = reverse . dropWhile isSpace


-- Generate stub code for RPC:
remotable ['parfib1, 'parfib0]

main = do 
    args <- getArgs
    let (version, trans_, size, cutoff1, cutoff2) = case args of 
            []        -> ("master", "pipes", 10, 1, 1)
            [v]       -> (v,        "pipes", 10, 1, 1)
            [v,t]     -> (v,         t,      10, 1, 1)
            [v,t,n]   -> (v,         t,  read n, 1, 1)
            [v,t,n,c] -> (v,         t,  read n, read c, 1)
            [v,t,n,c1,c2] -> (v,     t,  read n, read c1, read c2)
        trans = parse trans_
        parse "tcp"   = TCP
	parse "pipes" = Pipes

    putStr$ "Running parfib with settings: "
    putStrLn$ show (version, trans_, size, cutoff1, cutoff2)

    case version of 
        "slave" -> runParSlaveWithTransport [__remoteCallMetaData] trans
        "master" -> do 

                       mv    <- newEmptyMVar
                       start <- newEmptyMVar
                       -- Block until we start up the par computation:
                       forkIO $ do takeMVar mv 
				   getCurrentTime >>= putMVar start

		       ans <- (runParDistWithTransport [__remoteCallMetaData] trans
			       (do liftIO$ putMVar mv ()
				   parfib1 (size,cutoff1,cutoff2)) :: IO FibType)
                       end    <- getCurrentTime
		       putStrLn $ "Final answer: " ++ show ans
                       start' <- takeMVar start
                       putStrLn $ "SELFTIMED: "++ show ((fromRational $ toRational $ diffUTCTime end start') :: Double)

		       putStrLn $ "Calling SHUTDOWN..."
                       shutdownDist
		       putStrLn $ "... returned from shutdown, apparently successful."

        str -> error$"Unhandled mode: " ++ str

{-

[2012.03.10]

This seems to be working reasonably well now.
I'm running a three-tier parfib (like 45 30 20).


But parfib of 50 (50 40 30) eventually hits the infamous resource vanished problem:

     [distmeta_M ThreadId 6] [rcvdmn]   DONE running stolen par work.
    Exception inside child thread "spawned Par worker": writev: resource vanished (Connection reset by peer)
    parfib_dist.exe: writev: resource vanished (Connection reset by peer)
    Exception inside runParDist: thread blocked indefinitely in an MVar operation
    parfib_dist.exe: thread blocked indefinitely in an MVar operation
    real    8m46.413s
    user    26m33.648s
    sys     8m17.762s
    [rrnewton@slate ~/monad-par/



 -}
