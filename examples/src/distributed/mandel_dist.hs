{-# LANGUAGE BangPatterns, CPP, TemplateHaskell #-}

import Control.Monad
import Control.Seq
import Control.DeepSeq
import Control.Exception
import Data.Complex
import System.Environment
import System.IO
import qualified Control.Monad.Par.AList as A
import qualified Control.Monad.Par.Combinator as C
#ifdef PARSCHED 
import PARSCHED
#else
-- import Control.Monad.Par
-- import Control.Monad.Par.Meta.Dist
import Control.Monad.Par.Meta.DistSMP
#endif


-- For Dist version:
import RPC.Closure  (Closure(Closure))
import RPC.Encoding (Payload, Serializable, serialDecodePure, serialEncodePure,
			 getPayloadContent, getPayloadType)
import qualified RPC.Reg as Reg
import RPC.Call (mkClosureRec, remotable)
-- import Control.Monad.Par.Meta.Dist.Combinator (InclusiveRange(InclusiveRange), parMapReduceRange)
import Data.IORef (readIORef)
import Data.Maybe (fromJust)
import Control.Monad.IO.Class (liftIO)
import GHC.Conc (numCapabilities)

#ifdef WRITE_IMAGE
import Codec.Picture  -- JuicyPixels
import qualified Data.Vector.Storable as V
#endif

----------------------------------------
-- Duplicated code from Combinator.hs:

-- How many tasks per process should we aim for?  Higher numbers
-- improve load balance but put more pressure on the scheduler.
auto_partition_factor :: Int
auto_partition_factor = 4

data InclusiveRange = InclusiveRange Int Int


splitInclusiveRange :: Int -> (Int, Int) -> [(Int, Int)]
splitInclusiveRange pieces (start,end) =
  map largepiece [0..remain-1] ++
  map smallpiece [remain..pieces-1]
 where
   len = end - start + 1 -- inclusive [start,end]
   (portion, remain) = len `quotRem` pieces
   largepiece i =
       let offset = start + (i * (portion + 1))
       in (offset, offset + portion)
   smallpiece i =
       let offset = start + (i * portion) + remain
       in (offset, offset + portion - 1)

----------------------------------------------------------------------------------------------------

-- | The core scalar loop:
mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0
  where   
   fn = magnitude
   loop i !z
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c)

--type Bounds = (Int,Int, Int,Int, Int,Int, Int)
type Bounds = (Double,Double, Double,Double, Int,Int, Int)
--            (minX, minY, maxX, maxY, winX, winY, max_depth)


-- {-# INLINE mandelStep #-}

-- Evaluate at a given position, with scaling.
mandelStep :: Bounds -> Int -> Int -> Int
mandelStep (minX, minY, maxX, maxY, winX, winY, max_depth) i j = mandel max_depth z
    where z = ((fromIntegral j * r_scale) / fromIntegral winY + minY) :+
	      ((fromIntegral i * c_scale) / fromIntegral winX + minX)
	  r_scale  =  maxY - minY  :: Double
	  c_scale =   maxX - minX  :: Double

-- Create a row and return it as a singleton:
distBuilder :: Bounds -> Int -> Par (A.AList [Int])
distBuilder bnds@(_,_, _,_, winX,_, _) y = do 
   let l = [ mandelStep bnds y x | x <- [0.. winX-1] ] 
   deepseq l (return (A.singleton l))


----------------------------------------------------------------------------------------------------


runMandel :: Double -> Double -> Double -> Double -> Int -> Int -> Int -> Par (A.AList [Int])
runMandel minX minY maxX maxY winX winY max_depth = do
 parBuildAList (InclusiveRange 0 (winY-1)) bounds
  where
    bounds = (minX, minY, maxX, maxY, winX, winY, max_depth)

-- | Default entrypoint with default bounds:
simple x y depth = runMandel (-2) (-2) 2 2 x y depth

----------------------------------------------------------------------------------------------------

-- The "mkClosure" mechanism is not sufficiently robust to support the
-- types we'd like to use.  Hence the below -- an inlined and
-- specialized version of parMapReduceRange:
parBuildAList :: InclusiveRange -> Bounds -> Par (A.AList [Int])
parBuildAList (InclusiveRange start end) bounds =
   pmrr_loop(length segs, segs, bounds)
 where
  segs = splitInclusiveRange (auto_partition_factor * numCapabilities) (start,end)

pmrr_loop :: (Int, [(Int,Int)], Bounds) -> Par (A.AList [Int])
pmrr_loop( n, segs, bounds ) =
  case segs of 
   [(st,en)] -> 
     -- Execute serially:
     let mapred a b = do 
                         x <- distBuilder bounds b;
			 return (A.append a x)
     in foldM mapred A.empty [st..en]

   segs -> 
     let half = n `quot` 2
	 (left,right) = splitAt half segs in
     do 
        l  <- longSpawn$ $(mkClosureRec 'pmrr_loop)  
	                  (half, left, bounds )
        r  <- pmrr_loop (n-half, right, bounds)
	l' <- get l
	return (A.append l' r)



-- Generate stub code for RPC:
remotable ['pmrr_loop]


----------------------------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

-- A meaningless checksum.
mandelCheck :: A.AList [Int] -> Int -> Int -> Int
mandelCheck als max_col max_depth = loop 0 als 0
 where 
 loop i als !sum | A.null als = sum
 loop i als !sum = loop (i+1) (A.tail als)
		        (loop2 i 0 (A.head als) sum)
 loop2 i j []    !sum = sum
 loop2 i j (h:t) !sum | h == max_depth = loop2 i (j+1) t (sum + i*max_col + j)
		      | otherwise      = loop2 i (j+1) t  sum
	      
main = do role : args <- getArgs

          let (transport, x,y,depth) = 
		case args of
		 []      -> ("pipes",3,3,3)
		 [trans] -> (trans,  3,3,3)
		 [trans,x,y,depth] -> 
		       (trans,read x, read y, read depth)

          let trans = case transport of 
		        "tcp" -> TCP 
			"pipes" -> Pipes

          case role of 
	    "slave" -> runParSlaveWithTransport [__remoteCallMetaData] trans
	    "master" -> do 
			   ls <- runParDistWithTransport [__remoteCallMetaData] trans
			                                 (simple x y depth)
#ifdef WRITE_IMAGE
			   writePng "mandel_image.png" (makeImage (fromIntegral x) (fromIntegral y) depth ls)
                           putStrLn$ "File written."
#endif
			   putStrLn$ "Spot check: " ++ show (mandelCheck ls y depth)
	    str -> error$"Unhandled mode: " ++ str
