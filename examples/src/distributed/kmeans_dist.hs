-- K-Means sample from "Parallel and Concurrent Programming in Haskell"
-- Simon Marlow
-- with modifications for benchmarking: erjiang
--

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O2 -ddump-splices #-}
import System.IO
import System.IO.Unsafe
import Data.IORef
import KMeansCommon
import Control.Applicative
import Control.Monad.IO.Class (liftIO)
import Data.Array
import Text.Printf
import Data.Data
import Data.List
import Data.Function
import qualified Data.Serialize as Ser
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Data.Vector.Storable.Serialize
import Debug.Trace
import Control.Parallel.Strategies as Strategies
import Control.Monad.Par.Meta.DistSMP 
        (longSpawn, Par, get, spawn, runParDistWithTransport,
	 runParSlaveWithTransport, WhichTransport(Pipes, TCP), shutdownDist, readTransport)
import Control.DeepSeq
import System.Environment
import System.Random.MWC
import Data.Time.Clock
import Control.Exception
import Control.Monad
import RPC.Call (mkClosure, mkClosureRec, remotable)
import Data.ByteString (readFile)

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

{-# NOINLINE pointData #-}
pointData :: IORef (Maybe (V.Vector (V.Vector Point)))
pointData = unsafePerformIO $ newIORef Nothing

loadPoints :: String -> IO (V.Vector (V.Vector Point))
loadPoints filename = do
  bin <- Data.ByteString.readFile filename
  return $ case Ser.decode bin of
              Left err -> error $ "Could not deserialize data: "++err
              Right d -> V.fromList d :: V.Vector (V.Vector Point)

getChunk id = do
  dat <- readIORef pointData
  return $ case dat of
    Just chunks -> chunks V.! id
    Nothing -> error "Point data not loaded!"

-- -----------------------------------------------------------------------------
-- K-Means: repeatedly step until convergence (Par monad)

splitChunks :: (Int, Int, [Cluster]) -> Par [Cluster]
splitChunks (n0, nn, clusters) =
  case nn - n0 of
    0 -> kmeans_chunk clusters nn
    1 -> do
--           liftIO $ printf "local branch\n"
           lx <- spawn $ kmeans_chunk clusters n0
           rx <- spawn $ kmeans_chunk clusters nn
           l <- get lx
           r <- get rx
           return $ reduce nClusters [l, r]
    otherwise -> do
--           liftIO $ printf "longSpawn branch\n"
           lx <- longSpawn $ $(mkClosureRec 'splitChunks) (n0, (halve n0 nn), clusters)
           rx <- longSpawn $ $(mkClosureRec 'splitChunks) ((halve n0 nn), nn, clusters)
           l <- get lx
           r <- get rx
           return $ reduce nClusters [l, r]

{-# INLINE halve #-}
halve :: Int -> Int -> Int
halve n0 nn = n0 + (div (nn - n0) 2)

-- doChunks :: Int -> Int -> [Cluster] -> Par [[Cluster]]
-- -- parMap f xs = mapM (spawnP . f) xs >>= mapM get
-- doChunks n chunkSize clusters = mapM (spawn . return . (kmeans_chunk chunkSize clusters)) [0..(n-1)]
--   >>= mapM get


kmeans_chunk :: [Cluster] -> Int -> Par [Cluster]
kmeans_chunk clusters id = do
  points <- liftIO $ getChunk id
  return $ step clusters points

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

kmeans_par :: [Cluster] -> Int -> Par [Cluster]
kmeans_par clusters nChunks = do
  let
      loop :: Int -> [Cluster] -> Par [Cluster]
      loop n clusters | n > tooMany = do liftIO (printf "giving up."); return clusters
      loop n clusters = do
        liftIO $ hPrintf stderr "iteration %d\n" n
     -- hPutStr stderr (unlines (map show clusters))
        clusters' <- splitChunks (0, nChunks-1, clusters)

        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  --
  loop 0 clusters  

main = do
  args <- getArgs
  case args of
   ["master", trans, filename] -> do
     pts <- loadPoints filename
     writeIORef pointData (Just pts)
     clusters <- mapM genCluster [0..nClusters-1]
     printf "%d clusters generated\n" (length clusters)
     t0 <- getCurrentTime
     final_clusters <- runParDistWithTransport 
                         [__remoteCallMetaData] 
                         (parse_trans trans)
                         $ kmeans_par clusters (V.length pts)
     t1 <- getCurrentTime
     print final_clusters
     printf "SELFTIMED %.2f\n" (realToFrac (diffUTCTime t1 t0) :: Double)
   ["slave", trans, filename] -> do
     pts <- loadPoints filename
     writeIORef pointData (Just pts)
     runParSlaveWithTransport [__remoteCallMetaData] (parse_trans trans)
   ["test", filename] -> do
     printf "Compiled with support for %d-dim points\n" vectorSize
     pts <- loadPoints filename
     printf "%d chunks of %d points each loaded\n" (V.length pts) (V.length (pts V.! 0))
   otherwise -> error "Usage: kmeans_dist.exe (master|slave) (tcp|pipes) datafile"
  shutdownDist

parse_trans "tcp" = TCP
parse_trans "pipes" = Pipes
parse_trans _ = error "chicken"
