{-# LANGUAGE BangPatterns #-}
import System.Environment
import Control.Monad
import Control.Seq
import Data.Complex
import System.IO
import Debug.Trace
import Control.DeepSeq

import Control.Monad.Par

import Control.Monad.Par.AList

import PortablePixmap

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0
  where   
   fn = magnitude
   loop i !z
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c)

-- data AList a = ANil | ASing a | Append (AList a) (AList a) | AList [a]
-- append ANil r = r
-- append l ANil = l -- **
-- append l r    = Append l r

-- toList :: AList a -> [a]
-- toList a = go a []
--  where go ANil         rest = rest
--        go (ASing a)    rest = a : rest
--        go (Append l r) rest = go l $! go r rest
--        go (AList xs)   rest = xs ++ rest


-- Divide-and-conquer traversal of a dense input domain (a range).
parTreeLike :: Int -> Int -> (Int -> Par a) -> Par (AList a)
parTreeLike min max fn
 | max - min <= threshold = do
      l <- mapM fn [min..max]
      seqList r0 l `seq` return (AList l)
 | otherwise  = do
      rght <- spawn_ $ parTreeLike (mid+1) max fn
      left <- spawn_ $ parTreeLike min mid fn
      r <- get rght
      l <- get left
      return (l `append` r)
    where
      mid  = min + ((max - min) `quot` 2)

threshold = 1


runMandel :: Double -> Double -> Double -> Double -> Int -> Int -> Int -> Par PixMap
runMandel minX minY maxX maxY winX winY max_depth = do
  l <- parTreeLike 0 (winY-1) $ \y -> do
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
