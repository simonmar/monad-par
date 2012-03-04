{-# LANGUAGE CPP #-}

import Control.Applicative
import Control.Exception (evaluate)

import Control.Monad.Par.Meta.SMPMergeSort
import Foreign.CUDA (sync)

import qualified Data.Vector.Storable as V
import Data.Word (Word8, Word32)

import System.Environment
import System.Random.MWC    

import Control.DeepSeq
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Criterion.Main hiding (run)

import Data.Time.Clock
import Text.Printf

-- | Generate a random vector of unsigned integers where the length
-- given is multiplied by 1024 to meet the constraints of the CUDA
-- merge sort.
mkRandomVec :: Int -> IO (V.Vector Word32)
mkRandomVec k = withSystemRandom $ \g -> uniformVector g (k*1024) :: IO (V.Vector Word32)

parMergeSort :: V.Vector Word32 -> V.Vector Word32
parMergeSort v = runPar $ get =<< spawnMergeSort v

prop_parSortCorrect :: Positive Word8 -> Property       
prop_parSortCorrect k = monadicIO $ do
  unsorted <- run $ mkRandomVec (fromIntegral k)
  let sorted = sort $!! V.toList unsorted
  assert $ sorted == V.toList (parMergeSort unsorted)



main = do args <- getArgs
          case args of
            ["test"] -> quickCheck prop_parSortCorrect
            ("criterion":(k:args')) -> do
              v <- mkRandomVec (read k)
              withArgs args' $ defaultMain [
                  bench "parMergeSort" $ whnfIO $ (V.head . parMergeSort) <$> mkRandomVec (read k)
                ]
            [k] -> do
              sync
              v <- mkRandomVec (read k)
              () <- evaluate $ V.foldl' (flip seq) () v
              start <- getCurrentTime
              evaluate (parMergeSort v)
              end <- getCurrentTime
              let runningTime = ((fromRational $ toRational $ diffUTCTime end start) :: Double)
              printf "SELFTIMED: %s\n" (show runningTime)
            _ -> error "usage: cudaParMerge [test | criterion k | k]"