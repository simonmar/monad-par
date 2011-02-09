{-# LANGUAGE BangPatterns #-}
import System.Environment
import Control.Monad
import Control.Seq

import Control.Monad.Par

nqueens :: Int -> Par [[Int]]
nqueens nq = step 0 []
  where
    threshold = 5

    step :: Int -> [Int] -> Par [[Int]]
    step !n b
       | n >= threshold = return (iterate gen [b] !! (nq - n))
       | otherwise = do
          rs <- parMap (step (n+1)) (gen [b])
          return (concat rs)

    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: [[Int]] -> [[Int]]
    gen bs = [ (q:b) | b <- bs, q <- [1..nq], safe q 1 b ]


main = do
  [n] <- fmap (fmap read) getArgs
  print (length (runPar (nqueens n)))
