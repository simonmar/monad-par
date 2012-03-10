{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- Embarassingly parallel.
-- If this doesn't get a speedup nothing will!

-- Note: This program is an example of a dependence on "put" being
-- strict.  If it were not the real work would be deferred until after
-- the parallel computation is finished!

import GHC.Conc
import Debug.Trace
import Control.Monad
-- import Control.Monad.IO.Class (liftIO)

import Control.Exception (evaluate)
import System.Environment
import qualified Control.Monad.State.Strict as S 
import qualified Data.ByteString.Char8 as BS

import Control.Monad.Par.Scheds.Direct (Par, spawn_, get, runPar)
import Control.Monad.Par.Unsafe
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (yield)

-- Compute sum_n(1/n)
work :: Int -> Int -> Double -> Double
work offset 0 n = n
work offset i n | i `mod` 10000 == 0 = 
  unsafePerformIO $ do yield
--		       putStr "."
		       return (work offset (i-1) (n + 1 / fromIntegral (i+offset)))
work offset (!i) (!n) = work offset (i-1) (n + 1 / fromIntegral (i+offset))

-- runit :: (Double,Int) -> IO ()
runit :: Int -> Int -> IO ()
runit total partitions = evaluate $ runPar $ 
   do 
      prnt$ "Running embarassingly parallel benchmark."
      prnt$ "Running "++ show total ++" total iterations"
      prnt$ "Spawning "++show partitions++" tasks..."
      results <- mapM (spawn_ . kernel) [0 .. partitions-1] 
      prnt "Done initializing."

      final <- foldM (\ acc (ind,iv) -> 
		      do prnt$ "  Retrieving output "++ show ind ++": "
			 n <- get iv
			 prnt$ show n
			 return (acc + n))
		  0.0 (zip [0..] results)
      prnt$ "Final Output: " ++ show final
 where  
  oneshare = total `quot` partitions
  kernel jid =
     do 
        tid <- io myThreadId 
	prnt$ (show tid++" job "++show jid++":  About to do a chunk of work ("++ show oneshare ++" iterations)...")
        res <- io$ evaluate $ work (oneshare * jid) oneshare 0.0
	prnt$ (show tid++"   job "++show jid++":  done with work (result "++ show res ++")")
        return res

prnt :: String -> Par ()
prnt = io . BS.putStrLn . BS.pack 

-- io :: IO a -> Par a
io :: ParUnsafe p iv => IO a -> p a
io act = unsafeParIO act

main = do args <- getArgs 
	  case args of 
	      []        -> runit  (50*1000*1000) numCapabilities
	      -- The input is a power of 10:
	      [n]       -> runit  (round (10 ** read n)) numCapabilities
	      [n,parts] -> runit  (round (10 ** read n)) (read parts)
