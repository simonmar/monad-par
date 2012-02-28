{-# LANGUAGE BangPatterns, CPP #-}

import Control.Monad
import Control.Seq
import Control.DeepSeq
import Control.Exception
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

#ifdef WRITE_IMAGE
import Codec.Picture  -- JuicyPixels
import qualified Data.Vector.Storable as V
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


#ifdef WRITE_IMAGE
-- makeImage :: Integer -> Integer -> Int -> A.AList [Int] -> Image PixelRGB8 
makeImage :: Int -> Int -> Int -> A.AList [Int] -> Image PixelRGB8 
makeImage x y depth ls =
  -- This is a quite silly series of intermediate structures:
  Image x y (V.fromList $ concat $ 
	     Prelude.map prettyRGB $
	     concat$ A.toList ls)
 where 
   prettyRGB s = 
      let t = (fromIntegral (depth - s)) in 
--      PixelRGB8 (fromIntegral s) t t 
      [fromIntegral s, t, t]
#endif

simple x y depth = runMandel (-2) (-2) 2 2 x y depth


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
		       -- (1024,1024,256)
                       -- Note, this should do something very *quick* when called with no args:
                       (3,3,3)

		 [x,y,depth] -> 
    --		   simple (read x) (read y) (read depth)
		       (read x, read y, read depth)

		 -- [minX,minY,maxX,maxY,winX,winY,depth] ->
		 --    runPar $ 
		 -- 	    runMandel (read minX) (read minY)
		 -- 		      (read maxX) (read maxY)
		 -- 		      (read winX) (read winY) (read depth)

          let ls = runPar$ simple x y depth

#ifdef WRITE_IMAGE
	  writePng "mandel_image.png" (makeImage (fromIntegral x) (fromIntegral y) depth ls)
	  putStrLn$ "File written."
#endif
          putStrLn$ "Spot check: " ++ show (mandelCheck ls y depth)
