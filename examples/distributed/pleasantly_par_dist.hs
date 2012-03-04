{-# LANGUAGE BangPatterns, OverloadedStrings, TemplateHaskell #-}

-- Embarassingly parallel.
-- If this doesn't get a speedup nothing will!

-- Note: This program is an example of a dependence on "put" being
-- strict.  If it were not the real work would be deferred until after
-- the parallel computation is finished!

import GHC.Conc
import Data.Char            (isSpace)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Exception (evaluate)
-- import System.Environment
import qualified Control.Monad.State.Strict as S 
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Par.Meta.Dist (longSpawn, Par, get, shutdownDist, WhichTransport(Pipes,TCP),
				    runParDistWithTransport, runParSlaveWithTransport)
import Control.Monad.Par.Unsafe
import Remote2.Call (mkClosureRec, remotable)
import System.Process       (readProcess)
import System.Posix.Process (getProcessID)
import DistDefaultMain (defaultMain)

--------------------------------------------------------------------------------

-- Compute sum_n(1/n)
work :: Int -> Int -> Double -> Double
work offset 0 n = n
work offset (!i) (!n) = work offset (i-1) (n + 1 / fromIntegral (i+offset))

kernel :: (Int, Int) -> Par Double
kernel (oneshare,jid) =
   do 
      tid <- io myThreadId 
      mypid <- io getProcessID
      host  <- io hostName
      let tag = show (host,"PID "++show mypid,tid) ++ " job "++show jid
      prnt$ (tag++":  About to do a chunk of work ("++ show oneshare ++" iterations)...")
      res <- io$ evaluate $ work (oneshare * jid) oneshare 0.0
      prnt$ (tag++":  done with work (result "++ show res ++")")
      return res
 where 
  hostName = do s <- readProcess "hostname" [] ""
	        return (trim s)
  -- | Trim whitespace from both ends of a string.
  trim :: String -> String
  trim = f . f
     where f = reverse . dropWhile isSpace
--------------------------------------------------------------------------------

-- runit :: [Double] -> Par ()
runit :: (Double,Int) -> Par ()
runit (expt,partitions) = 
--    evaluate $ runPar $ 
   do 
      prnt$ "Running embarassingly parallel benchmark."
      prnt$ "Running "++ show total ++" total iterations"
      prnt$ "Spawning "++show partitions++" tasks..."
--      results <- mapM (spawn_ . (kernel oneshare)) [0 .. partitions-1] 
      results <- forM [0 .. partitions-1] $ \ jid -> 
          longSpawn$  $(mkClosureRec 'kernel) (oneshare,jid)
      prnt "Done initializing."

      final <- foldM (\ acc (ind,iv) -> 
		      do prnt$ "  Retrieving output "++ show ind ++": "
			 n <- get iv
			 prnt$ show n
			 return (acc + n))
		  0.0 (zip [0..] results)
      prnt$ "Final Output: " ++ show final
 where  
  total    = round (10 ** expt)
  oneshare = total `quot` partitions

prnt :: String -> Par ()
prnt = io . BS.putStrLn . BS.pack 

io :: IO a -> Par a
io act = liftIO act
-- io :: ParUnsafe p iv => IO a -> p a
-- io act = unsafeParIO act


-- Generate stub code for RPC:
remotable ['kernel]

-- partitions = numCapabilities

main = defaultMain [__remoteCallMetaData] runit 2
--                   ["7.5",show numCapabilities]
                   (\ ranks [s1,s2] -> 
		      return (case s1 of "" -> 7.5 
			                 _  -> read s1, 
			      case s2 of "" -> ranks * numCapabilities
					 _  -> read s2))
--                   [ \_     -> return 7.5
--		   , \ranks -> return ranks]

-- main = do args <- getArgs 
-- 	  (version, iters) <- case args of 
-- 		[]   -> ("master",50*1000*1000)
-- 		[v]  -> (v,       50*1000*1000)
-- 		-- The input is a power of 10:
-- 		[v,n] -> (v, round (10 ** read n))
--           let trans = parse trans_
--               parse "tcp"   = TCP
-- 	      parse "pipes" = Pipes

-- 	  case version of 
-- 	      "slave" -> runParSlaveWithTransport [__remoteCallMetaData] trans
-- 	      "master" -> do 
-- 			     putStrLn "Using non-thresholded version:"
-- 			     ans <- (runParDistWithTransport [__remoteCallMetaData] trans
-- 				     (parfib1 size) :: IO FibType)
-- 			     putStrLn $ "Final answer: " ++ show ans
-- 			     putStrLn $ "Calling SHUTDOWN..."
-- 			     shutdownDist
-- 			     putStrLn $ "... returned from shutdown, apparently successful."

-- 	      str -> error$"Unhandled mode: " ++ str

