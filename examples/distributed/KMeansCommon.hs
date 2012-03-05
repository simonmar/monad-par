{-# LANGUAGE DeriveDataTypeable #-}
module KMeansCommon where

import Data.List (foldl')
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector         as V
import Control.DeepSeq
import Control.Monad
import System.Random.MWC

-- data Point = Point {-#UNPACK#-}!Double {-#UNPACK#-}!Double deriving (Show,Read,Typeable,Data,Eq)

-- instance Binary Point where put (Point a b) = put a>>put b
--                              get = do a<-get
--                                       b<-get
--                                       return $ Point a b
--


-- change vectorSize to control how many dimensions Point has and then
-- recompile
vectorSize :: Int
vectorSize = 50

type Point = U.Vector Double

data Cluster = Cluster
               {
                  clId    :: {-#UNPACK#-}!Int,
                  clCount :: {-#UNPACK#-}!Int,
                  clSum   :: !Point,
                  clCent  :: !Point
               } deriving (Show,Read,Typeable,Data,Eq)

instance NFData Cluster  -- default should be fine

sqDistance :: Point -> Point -> Double
sqDistance p1 p2 =
   foldl' (\a i -> a + ((p1 U.! i) - (p2 U.! i)) ^ 2) 0 [0..vectorSize-1] :: Double

makeCluster :: Int -> [Point] -> Cluster
makeCluster clid pts
   = Cluster { clId = clid,
               clCount = count,
               clSum = vecsum,
               clCent = centre
             }
   where vecsum = foldl' addPoint zeroPoint pts
         centre = U.map (\a -> a / fromIntegral count) vecsum
         count = length pts

combineClusters c1 c2 =
  Cluster {clId = clId c1,
           clCount = count,
           clSum = vecsum,
           clCent = centre }
  where count = clCount c1 + clCount c2
        centre = U.map (\a -> a / fromIntegral count) vecsum
        vecsum = addPoint (clSum c1) (clSum c2)

addPoint p1 p2 = U.imap (\i v -> v + (p2 U.! i)) p1
zeroPoint = U.replicate vectorSize 0

genChunk :: Int -> Int -> IO (V.Vector Point)
genChunk id n = do
  g <- initialize $ U.singleton $ fromIntegral id
  V.replicateM n (U.replicateM vectorSize (uniform g))

-- getPoints :: FilePath -> IO [Point]
-- getPoints fp = do c <- readFile fp
--                   return $ read c

genCluster :: Int -> IO Cluster
genCluster id = do
  g <- initialize (U.singleton (fromIntegral $ -1 * id))
  centre <- U.replicateM vectorSize (uniform g)
  return (Cluster id 0 centre centre)

getClusters :: FilePath -> IO [Cluster]
getClusters fp = do c <- readFile fp
                    return $ read c

--readPoints :: FilePath -> IO [Point]
--readPoints f = do
--  s <- B.readFile f
--  let ls = map B.words $ B.lines s
--      points = [ Point (read (B.unpack sx)) (read (B.unpack sy))
--               | (sx:sy:_) <- ls ]
--
--  return points
