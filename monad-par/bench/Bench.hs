{-# LANGUAGE RankNTypes, BangPatterns #-}

module Main where

import Control.Monad.Par
import Control.Monad (forM_)
import Criterion.Main
import Criterion.Types (Benchmarkable(Benchmarkable))


-- | Helper for benchmarking par-monad actions:
benchPar :: Par () -> Benchmarkable
benchPar par = Benchmarkable $ \ iters -> 
  runParIO $ 
    for_  1 (fromIntegral iters) $ \_ -> do
       par
{-# INLINE benchPar #-}

main = defaultMain [
  bgroup "monad-par"
    [ bench "NOOP" $ benchPar (return ())
    , bench "fork" $ benchPar (fork (return ()))
    , bench "new"  $ benchPar  (do new; return ())
    , bench "newFull"     $ benchPar (do iv <- newFull (3::Int); return ())
    , bench "newFull-get" $ benchPar (do iv <- newFull (3::Int); get iv; return ())
    , bench "new-put"     $ benchPar (do iv <- new; put iv (3::Int); return ())
    , bench "new-put-get" $ benchPar (do iv <- new; put iv (3::Int); get iv; return ())
    , bench "new-fork-get" $ benchPar (do iv <- new; fork (put iv (3::Int)); get iv; return ())
    ]
  ]

-- My own forM for inclusive numeric ranges (not requiring deforestation optimizations).
{-# INLINE for_ #-}
for_ :: Monad m => Int -> Int -> (Int -> m ()) -> m ()
for_ start end _fn | start > end = error "for_: start is greater than end"
for_ start end fn = loop start
  where
   loop !i | i > end  = return ()
	   | otherwise = do fn i; loop (i+1)
