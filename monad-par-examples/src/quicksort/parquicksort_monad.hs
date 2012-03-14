{-# LANGUAGE CPP #-}

-- A quicksort benchmark for monad-par

import System.Random
import System.Environment
import Control.Monad
import Data.Time.Clock
import Text.Printf
import Control.Monad.Par.AList as A
import Control.Exception
import Control.DeepSeq
import Data.Int
import Data.List as L
-- import Prelude (($), print, read)
-- import qualified Prelude as P

#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
-- import Control.Monad.Par.Scheds.Trace
#endif





-- TODO: Rewrite with AList.. lists are not good for this.

quicksortP :: A.AList Int -> Par (A.AList Int)
quicksortP A.ANil       = return empty
quicksortP (A.ASing x)  = return (singleton x)
quicksortP (A.AList ls) = error "implement me"

-- This quicksort always choses the pivot to be the first element:
quicksortP xs = 
-- quicksortP (A.Append hd tl) = 
  let pivot = A.head xs 
      rest  = A.tail xs in
  do
     let low  =  A.filter (<  pivot) rest
         high =  A.filter (>= pivot) rest

     lf <- spawn$ quicksortP low
     h  <-        quicksortP high
     l  <- get lf

     return (l `append` singleton pivot `append` h)
    

-- Another version.  TODO: Switch to this one:
-- | AList version of quicksort
aqs :: A.AList Int -> A.AList Int
aqs A.ANil = A.ANil
aqs (A.ASing x) = A.ASing x
aqs (A.AList xs) = undefined
aqs (A.Append xs ys) = low' `A.append` (pivot `A.cons` high')
  where pivot = A.head xs
        (low, high) = A.partition (< pivot) (A.Append (A.tail xs) ys)
        low' = aqs low
        high' = aqs high

prop_aqs :: [Int] -> Bool
prop_aqs xs = L.sort xs == (A.toList $ aqs (A.fromListBalanced xs))

-- TODO: replace with fromListBalanced:
-- | 'genRandoms' creates 2^N random numbers.
genRandoms :: Int -> StdGen -> AList Int
--genRandoms n = loop (mkStdGen 120) n 
genRandoms n g = loop g n 
 where
  loop rng 0 = ASing$ fst$ next rng
  loop rng n = let (r1,r2) = split rng in
               A.Append (loop r1 (n-1)) 
		        (loop r2 (n-1))

main = do args <- getArgs
          let size =
                case args of
                  []  -> 8
                  [n] -> (read n)

          g <- getStdGen          
          let rands = genRandoms size g
--          let rands = genRandoms size (mkStdGen 120)

          putStrLn$ "Quicksorting "++show size++" elements. First deepseq the rands:"
	  evaluate (deepseq rands ())
--          nfIO rands
-- 	  deepseq rands $ return ()

--           putStrLn$ "Length of rands: "++ show (A.length rands)
--           putStrLn$ "Length of filtered: "++ show (A.length (A.filter (>=0) rands))

          putStrLn "Executing monad-par based sort..."
          start <- getCurrentTime
          let sorted = runPar $ quicksortP rands
          putStr "Prefix of sorted list:\n  "
          print$ take 8 $ A.toList sorted
          end   <- getCurrentTime

          let runningTime = ((fromRational $ toRational $ diffUTCTime end start) :: Double)
          printf "Sorting AList took %0.3f sec.\n" runningTime
          putStrLn $ "SELFTIMED " ++ show runningTime
          when (size <= 4) $ do
            putStrLn$ "  Unsorted: " ++  show rands
            putStrLn$ "  Sorted  : " ++  show sorted
