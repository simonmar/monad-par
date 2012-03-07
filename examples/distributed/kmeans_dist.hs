-- K-Means sample from "Parallel and Concurrent Programming in Haskell"
-- Simon Marlow
-- with modifications for benchmarking: erjiang
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 -ddump-splices #-}
import System.IO
import System.IO.Unsafe
import KMeansCommon
import Data.Array
import Text.Printf
import Data.List
import Data.Function
import qualified Data.Vector as V
import Debug.Trace
import Control.Parallel.Strategies as Strategies
import Control.Monad.Par.Meta.Dist (longSpawn, Par, get, spawn, runParDistWithTransport,
  runParSlaveWithTransport, WhichTransport(Pipes, TCP), shutdownDist)
import Control.DeepSeq
import System.Environment
import Data.Time.Clock
import Control.Exception
import Control.Monad
import Remote2.Call (mkClosureRec, remotable)

nClusters = 4

-- -----------------------------------------------------------------------------
-- K-Means: repeatedly step until convergence (sequential)

-- kmeans_seq :: Int -> [Cluster] -> IO [Cluster]
-- kmeans_seq nclusters points clusters = do
--   let
--       loop :: Int -> [Cluster] -> IO [Cluster]
--       loop n clusters | n > tooMany = do printf "giving up."; return clusters
--       loop n clusters = do
--       --hPrintf stderr "iteration %d\n" n
--       --hPutStr stderr (unlines (map show clusters))
--         let clusters' = step nclusters clusters points
--         if clusters' == clusters
--            then do
--                printf "%d iterations\n" n
--                return clusters
--            else loop (n+1) clusters'
--   --
--   loop 0 clusters

tooMany = 50

-- -----------------------------------------------------------------------------
-- K-Means: repeatedly step until convergence (Par monad)

kmeans_par :: Int -> Int -> IO [Cluster]
kmeans_par nChunks chunkSize = do
  clusters <- mapM genCluster [0..nClusters-1]
  printf "%d clusters generated\n" (length clusters)
  let
      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters | n > tooMany = do printf "giving up."; return clusters
      loop n clusters = do
        hPrintf stderr "iteration %d\n" n
     -- hPutStr stderr (unlines (map show clusters))
        let
             clusters' = runParDistWithTransport $ splitChunks 0 nChunks chunkSize clusters

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  --
  final <- loop 0 clusters
  return final

splitChunks :: Int -> Int -> Int -> [Cluster] -> Par [Cluster]
splitChunks n0 nn chunkSize clusters =
  case nn - n0 of
    0 -> return $ kmeans_chunk chunkSize clusters nn
    1 -> do
           lx <- pval $ kmeans_chunk chunkSize clusters n0
           rx <- pval $ kmeans_chunk chunkSize clusters nn
           l <- get lx
           r <- get rx
           return $ reduce nClusters [l, r]
    otherwise -> do
           lx <- spawn     $ splitChunks n0 (halve n0 nn) chunkSize clusters
           rx <- longSpawn $ splitChunks (halve n0 nn) nn chunkSize clusters
           l <- get lx
           r <- get rx
           return $ reduce nClusters [l, r]

{-# INLINE halve #-}
halve :: Int -> Int -> Int
halve n0 nn = n0 + (div (nn - n0) 2)

doChunks :: Int -> Int -> [Cluster] -> Par [[Cluster]]
-- parMap f xs = mapM (spawnP . f) xs >>= mapM get
doChunks n chunkSize clusters = mapM (spawn . return . (kmeans_chunk chunkSize clusters)) [0..(n-1)]
  >>= mapM get


kmeans_chunk :: Int -> [Cluster] -> Int -> [Cluster]
kmeans_chunk chunkSize clusters id =
  let points = unsafePerformIO $ genChunk id chunkSize in
    step clusters points

-- -----------------------------------------------------------------------------
-- Perform one step of the K-Means algorithm

reduce :: Int -> [[Cluster]] -> [Cluster]
reduce nclusters css =
  concatMap combine $ elems $
     accumArray (flip (:)) [] (0,nclusters) [ (clId c, c) | c <- concat css]
 where
  combine [] = []
  combine (c:cs) = [foldr combineClusters c cs]

{-# INLINE step #-}
step :: [Cluster] -> (V.Vector Point) -> [Cluster]
step clusters points
   = makeNewClusters (assign clusters points)

-- assign each vector to the nearest cluster centre
assign :: [Cluster] -> (V.Vector Point) -> Array Int [Point]
assign clusters points =
    accumArray (flip (:)) [] (0, nclusters-1)
       [ (clId (nearest p), p) | p <- V.toList points ]
  where
    nclusters = (length clusters)
    nearest p = fst $ minimumBy (compare `on` snd)
                          [ (c, sqDistance (clCent c) p) | c <- clusters ]

makeNewClusters :: Array Int [Point] -> [Cluster]
makeNewClusters arr =
  filter ((>0) . clCount) $
     [ makeCluster i ps | (i,ps) <- assocs arr ]
                        -- v. important: filter out any clusters that have
                        -- no points.  This can happen when a cluster is not
                        -- close to any points.  If we leave these in, then
                        -- the NaNs mess up all the future calculations.

remotable ['splitChunks]


main = do
  args <- getArgs
  t0 <- getCurrentTime
  final_clusters <- case args of
-- ["strat",nChunks, chunkSize] -> kmeans_strat (read npts) nClusters clusters
   ["master", trans, nChunks, chunkSize] ->
     kmeans_par trans (read nChunks) (read chunkSize)
   ["slave", trans] -> runParSlaveWithTransport [__remoteCallMetaData] (parse_trans trans)
   _other -> kmeans_par 2 14
  t1 <- getCurrentTime
  shutdownDist
  print final_clusters
  printf "SELFTIMED %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)

parse_trans "tcp" = TCP
parse_trans "pipes" = Pipes
