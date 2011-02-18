{-# LANGUAGE BangPatterns, CPP #-}
import System.Environment
import Control.Monad
import Control.Seq
import Data.Complex
import System.IO
import Debug.Trace
import Control.DeepSeq

import Control.Monad.Par

import PortablePixmap
import Control.Monad.Par.AList

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0
  where   
   fn = magnitude
   loop i !z
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c)


threshold = 1

runMandel :: Double -> Double -> Double -> Double -> Int -> Int -> Int -> Par PixMap
runMandel minX minY maxX maxY winX winY max_depth = do
  l <- parBuildM threshold 0 (winY-1) $ \y -> do
          let l = [ mandelStep y x | x <- [0.. winX-1] ]
          deepseq l (return l)
  return (createPixmap (fromIntegral winX) (fromIntegral winY)
                       (fromIntegral max_depth)
                       (map prettyRGB (concat (toList l))))
  where
    mandelStep i j = mandel max_depth z
        where z = ((fromIntegral j * r_scale) / fromIntegral winY + minY) :+
                  ((fromIntegral i * c_scale) / fromIntegral winX + minX)

    r_scale  =  maxY - minY  :: Double
    c_scale =   maxX - minX  :: Double

    prettyRGB::Int -> (Int,Int,Int)
    prettyRGB s = let t = (max_depth - s) in (s,t,t)

main = do args <- getArgs
          hSetBinaryMode stdout True
          case args of
           []      -> print $ runPar $ runMandel (-2) (-2) 2 2 3 3 3
           [minX,minY,maxX,maxY,winX,winY,depth] ->
              print $ runPar $ runMandel (read minX) (read minY)
                                       (read maxX) (read maxY)
                                       (read winX) (read winY) (read depth)
