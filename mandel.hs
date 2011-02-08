{-# LANGUAGE BangPatterns #-}
import System.Environment
import Control.Monad
import Control.Seq
import Data.Complex
import System.IO
import Debug.Trace
import Control.DeepSeq

import ParCont
import PortablePixmap

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0
  where   
   fn = magnitude
   loop i !z
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c)




data AList a = ANil | ASing a | Append (AList a) (AList a) | AList [a]

append ANil r = r
append l ANil = l -- **
append l r    = Append l r

toList :: AList a -> [a]
toList a = go a []
 where go ANil         rest = rest
       go (ASing a)    rest = a : rest
       go (Append l r) rest = go l $! go r rest
       go (AList xs)   rest = xs ++ rest

parTreeLike :: Int -> Int -> (Int -> P a) -> P (AList a)
parTreeLike min max fn
 | max - min <= threshold = do
      l <- mapM fn [min..max]
      seqList r0 l `seq` return (AList l)
 | otherwise  = do
      rght <- forkR $ parTreeLike (mid+1) max fn
      left <- forkR $ parTreeLike min mid fn
      r <- get rght
      l <- get left
      return (l `append` r)
    where
      mid  = min + ((max - min) `quot` 2)

threshold = 1


runMandel :: Int -> Int -> Int -> P PixMap
runMandel max_row max_col max_depth = do
  l <- parTreeLike 0 (max_row-1) $ \y ->
          let l = [ mandelStep y x | x <- [0.. max_col-1] ] in
          deepseq l (return l)
  return (createPixmap (fromIntegral max_col) (fromIntegral max_row) (fromIntegral max_depth) (map prettyRGB (concat (toList l))))
  where
    mandelStep i j = mandel max_depth z
        where z = ((fromIntegral j * r_scale) / fromIntegral max_row + r_origin) :+
                  ((fromIntegral i * c_scale) / fromIntegral max_col + c_origin)

    r_origin = -2.0  :: Double
    r_scale  =  4.0  :: Double
    c_origin = -2.0  :: Double
    c_scale =   4.0  :: Double

    prettyRGB::Int -> (Int,Int,Int)
    prettyRGB s = let t = (max_depth - s) in (s,t,t)

main = do args <- getArgs
          hSetBinaryMode stdout True
          case args of
           []      -> print $ runP $ runMandel 3 3 3   -- Should output 24.
           [a,b,c] -> print $ runP $ runMandel (read a) (read b) (read c)
