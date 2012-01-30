{-# LANGUAGE BangPatterns, CPP #-}

import Control.Monad
import Control.Seq
import Control.DeepSeq
import Control.Exception
import PortablePixmap
import Data.Complex
import System.Environment
import System.IO
import Control.Monad.Par.AList as A
import qualified Control.Monad.Par.Combinator as C
#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
#endif

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0
  where   
   fn = magnitude
   loop i !z
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c)


threshold = 1

runMandel :: Double -> Double -> Double -> Double -> Int -> Int -> Int -> Par (AList [Int])
runMandel minX minY maxX maxY winX winY max_depth = do

  A.parBuildThreshM threshold (C.InclusiveRange 0 (winY-1)) $ \y -> 
       do
          let l = [ mandelStep y x | x <- [0.. winX-1] ]
          deepseq l (return l)

  where
    mandelStep i j = mandel max_depth z
        where z = ((fromIntegral j * r_scale) / fromIntegral winY + minY) :+
                  ((fromIntegral i * c_scale) / fromIntegral winX + minX)

    r_scale  =  maxY - minY  :: Double
    c_scale =   maxX - minX  :: Double


makeImage :: Integer -> Integer -> Int -> AList [Int] -> PixMap
makeImage x y depth ls =
  createPixmap x y depth 
   (Prelude.map prettyRGB (concat (toList ls)))
 where 
   prettyRGB :: Int -> (Int,Int,Int)
   prettyRGB s = let t = (depth - s) in (s,t,t)



simple x y depth = runMandel (-2) (-2) 2 2 x y depth
{-
simple x y depth = 
  runMandel 0 0 x' y' x y depth
 where 
  x' = fromIntegral x
  y' = fromIntegral y 
-}


--------------------------------------------------------------------------------

-- A meaningless checksum.
mandelCheck :: AList [Int] -> Int -> Int -> Int
mandelCheck als max_col max_depth = loop 0 als 0
 where 
 loop i als !sum | A.null als = sum
 loop i als !sum = loop (i+1) (A.tail als)
		        (loop2 i 0 (A.head als) sum)
 loop2 i j []    !sum = sum
 loop2 i j (h:t) !sum | h == max_depth = loop2 i (j+1) t (sum + i*max_col + j)
		      | otherwise      = loop2 i (j+1) t  sum
	      
main = do args <- getArgs

          let (x,y,depth) = 
		case args of
		 [] -> 
    --                 runPar $ simple 3 3 3
                       -- Defaults recommended by Simon Marlow
		       (1024,1024,256)

		 [x,y,depth] -> 
    --		   simple (read x) (read y) (read depth)
		       (read x, read y, read depth)

		 -- [minX,minY,maxX,maxY,winX,winY,depth] ->
		 --    runPar $ 
		 -- 	    runMandel (read minX) (read minY)
		 -- 		      (read maxX) (read maxY)
		 -- 		      (read winX) (read winY) (read depth)

          let ls = runPar$ simple x y depth
          when (False) $ do 
	     hnd <- openFile "mandel_image.ppm" WriteMode
	     hSetBinaryMode hnd True
	     hPrint hnd (makeImage (fromIntegral x) (fromIntegral y) depth ls)
	     hClose hnd

          putStrLn$ "Spot check: " ++ show (mandelCheck ls y depth)
