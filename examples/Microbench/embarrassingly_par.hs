{-# LANGUAGE BangPatterns, CPP #-}
-- Author: Ryan Newton 

-- Embarassingly parallel.
-- If this doesn't get a speedup nothing will!

-- Note: This program is an example of a program that depends
-- critically on "put" being strict.  If it were not the real work
-- would be deferred until after the parallel computation is finished!

import GHC.Conc
import Debug.Trace
import Control.Monad
import System.Environment

import Microbench.Flops
-- import Control.Monad.Par.Scheds.Trace
import Control.Exception
#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
#endif

puts = unsafeIO . putStr

runit total = runPar$ do

  let oneshare = total `quot` numCapabilities
      mystep jid = do 
        tid <- unsafeIO myThreadId 
	puts (show tid++" job "++show jid++":  About to do work chunk ("++ show oneshare ++" iterations)...\n")
--        let res = work (oneshare * jid) oneshare 0.0
	--tid2 <- S.lift$ myThreadId 
--	stepPutStr (show tid++"   job "++show jid++":  done with work (result "++ show res ++"), putting item...\n")
        return$ sillysum (oneshare * jid) oneshare 

--      fn jid = work (oneshare * jid) oneshare 0.0

  puts$ "Running embarassingly parallel benchmark.  Monad par variant: "++ show "hmm..." ++"\n"
  puts$ "Running "++ show total ++" total iterations\n"
  puts$ "Begin initialize.  Splitting work into "++show numCapabilities++" pieces\n"

  ivs <- mapM (spawn_ . mystep) [0 .. numCapabilities-1]

  puts$ "About to block on output:\n"
  final <- 
	    foldM (\ acc i -> 
		    do n <- get i
		       return (acc + n)) 
		  0.0 ivs
  puts$ "Final Output: " ++ show final ++"\n"
  return final


main = do args <- getArgs 
	  let x = loop args
          putStrLn$ "Done: "++ show x
  where 
    loop args = 
       case args of 
	   []  -> runit $ 50*1000*1000
	   [n] -> runit $ let num = read n in 
		          -- Here's a bit of a hack, if the input is inexact treat it as an exponent.
			  -- Otherwise as a plain scalar.
			  if num == fromIntegral (round num)
                          then round num
		          else round (10 ** read n)
--	   [trials, n] -> doTrials (read trials) (loop [n])
