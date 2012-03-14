{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}
module KMeansCommon where

import Control.Applicative
import Data.List (foldl')
import Data.Typeable (Typeable)
import Data.Data (Data)
import qualified Data.Serialize as Ser
import qualified Data.ByteString.Char8 as B
import qualified Data.Vector.Storable as SV
import qualified Data.Vector         as V
import Data.Vector.Serialize
import Data.Vector.Storable.Serialize
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
vectorSize = 100

type Point = SV.Vector Double

data Cluster = Cluster
               {
                  clId    :: {-#UNPACK#-}!Int,
                  clCount :: {-#UNPACK#-}!Int,
                  clSum   :: !Point,
                  clCent  :: !Point
               } deriving (Show,Read,Typeable,Data,Eq)

instance Ser.Serialize Cluster where
  put Cluster{ clId, clCount, clSum, clCent } =
    Ser.put clId >> Ser.put clCount >> Ser.put clSum >> Ser.put clCent
  get = Cluster <$> Ser.get <*> Ser.get <*> Ser.get <*> Ser.get

instance NFData Cluster  -- default should be fine

sqDistance :: Point -> Point -> Double
sqDistance p1 p2 =
   foldl' (\a i -> a + ((p1 SV.! i) - (p2 SV.! i)) ^ 2) 0 [0..vectorSize-1] :: Double

makeCluster :: Int -> [Point] -> Cluster
makeCluster clid pts
   = Cluster { clId = clid,
               clCount = count,
               clSum = vecsum,
               clCent = centre
             }
   where vecsum = foldl' addPoint zeroPoint pts
         centre = SV.map (\a -> a / fromIntegral count) vecsum
         count = length pts

combineClusters c1 c2 =
  Cluster {clId = clId c1,
           clCount = count,
           clSum = vecsum,
           clCent = centre }
  where count = clCount c1 + clCount c2
        centre = SV.map (\a -> a / fromIntegral count) vecsum
        vecsum = addPoint (clSum c1) (clSum c2)

addPoint p1 p2 = SV.imap (\i v -> v + (p2 SV.! i)) p1
zeroPoint = SV.replicate vectorSize 0

genChunk :: Int -> Int -> IO (V.Vector Point)
genChunk id n = do
  g <- initialize $ SV.singleton $ fromIntegral id
  V.replicateM n (SV.replicateM vectorSize (uniform g))

-- getPoints :: FilePath -> IO [Point]
-- getPoints fp = do c <- readFile fp
--                   return $ read c

genCluster :: Int -> IO Cluster
genCluster id = do
  g <- initialize (SV.singleton (fromIntegral $ -1 * id))
  centre <- SV.replicateM vectorSize (uniform g)
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
