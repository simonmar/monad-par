{-
 - Modified from code released with:
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -}
{-# LANGUAGE ExistentialQuantification
   , ScopedTypeVariables
   , BangPatterns
   , NamedFieldPuns 
   , RecordWildCards
   , FlexibleInstances
   , DeriveDataTypeable
   , MagicHash 
   , UnboxedTuples
   , CPP
  #-}
-- This is INCOMPATIBLE with CncPure..

-- Author: Chih-Ping Chen
-- Ported to Monad-par by Ryan Newton.

-- This program uses CnC to calculate the accelerations of the bodies in a 3D system.  

import Control.Monad
import Data.Int
import qualified Data.List as List
import qualified Data.Array as A
import GHC.Exts
import System.Environment
#ifdef PARSCHED 
import PARSCHED
#else
import Control.Monad.Par
#endif

type Float3D = (Float, Float, Float)
type UFloat3D = (# Float#, Float#, Float# #)


-- This step generates the bodies in the system.
genVector tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral tag

-- We are keeping the idiomatic Haskell version around as well for comparison: 
-- #define IDIOMATIC_VER

-- Only doing the O(N^2) part in parallel:
-- This step computes the accelerations of the bodies.       
compute :: A.Array Int Float3D -> A.Array Int (IVar Float3D) -> Int -> Par ()
compute vecList accels tag =
    do 
       let myvector = vecList A.! (tag-1)
       put (accels A.! tag) (accel myvector vecList)
       where 
             g = 9.8

             multTriple :: Float -> Float3D -> Float3D
             multTriple c ( x,y,z ) = ( c*x,c*y,c*z )

	     pairWiseAccel :: Float3D -> Float3D -> Float3D
             pairWiseAccel (x,y,z) (x',y',z') = let dx = x'-x
                                                    dy = y'-y
                                                    dz = z'-z
                                                    eps = 0.005
						    -- Performance degredation here:
						    distanceSq = dx*dx + dy*dy + dz*dz + eps
						    factor = 1/sqrt(distanceSq * distanceSq * distanceSq)

--                                                in multTriple factor (dx,dy,dz)
                                                in multTriple factor (dx,dy,dz)
#ifdef IDIOMATIC_VER
             sumTriples = foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)
	     accel vector vecList = multTriple g $ sumTriples $ List.map (pairWiseAccel vector) vecList
#else
-- Making this much less idiomatic to avoid allocation:
             (strt,end) = A.bounds vecList

             accel :: Float3D -> (A.Array Int Float3D) -> Float3D
	     accel vector vecList = 

             -- Manually inlining to see if the tuples unbox:
	        let (# sx,sy,sz #) = loop strt 0 0 0
		    loop !i !ax !ay !az
                      | i == end = (# ax,ay,az #)
		      | otherwise = 
                       let ( x,y,z )    = vector
			   ( x',y',z' ) = vecList A.! i

                           (# dx,dy,dz #) = (# x'-x, y'-y, z'-z #)
			   eps = 0.005
			   distanceSq = dx*dx + dy*dy + dz*dz + eps
			   factor = 1/sqrt(distanceSq * distanceSq * distanceSq)

			   (# px,py,pz #) = (# factor * dx, factor * dy, factor *dz #)

		       in loop (i+1) (ax+px) (ay+py) (az+pz)
		in ( g*sx, g*sy, g*sz )
#endif


run :: Int -> [Float3D]
run n = runPar $ 
        do 
	   vars <- sequence$ take n $ repeat new
--           accels  <- A.array (0,n-1) [ (i,) | i <- [0..n-1]]
	   -- Is there a better way to make an array of pvars?
           let accels = A.array (1,n) (zip [1..n] vars)

#ifdef IDIOMATIC_VER
           let initVecs = List.map genVector [1..n]
#else
           let initVecs = A.array (0,n-1) [ (i, genVector i) | i <- [0..n-1] ]
#endif
           
	   forM_ [1..n] $ \ t -> fork (compute initVecs accels t)

           sequence (List.map (\i -> get (accels A.! i)) [1..n])

	  
main = 
    do args <- getArgs 
       let accList = case args of 
                      []  -> run (3::Int)
		      [s] -> run (read s)
       putStrLn $ show (foldl (\sum (x,y,z) -> if x>0 then sum+1 else sum) 0 accList)


