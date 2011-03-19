{-# LANGUAGE BangPatterns, CPP #-}

module Main where

import Control.Monad
import Control.Monad.Par
import Control.Monad.Par.OpenList
import Control.DeepSeq

--import qualified Data.Array.Unboxed as U
import Data.Array.Unboxed as U
import Data.Array.CArray as C
import Data.Complex
import Data.Int
import Data.Word
import Data.List (intersperse)
import Data.List.Split (chunk)

import System.CPUTime
import System.CPUTime.Rdtsc
import GHC.Conc
import GHC.IO (unsafePerformIO, unsafeDupablePerformIO)
import Math.FFT (dft)
import Debug.Trace

type Stream a = IVar (IList a)

-- A windowed stream passes chunks of stream elements.
type WindowedStream a = Stream (Window a)

#define CARRAY
#ifdef CARRAY
type Window a = CArray Int a
#else
type Window a = U.UArray Int a
#endif

kern0 :: Window Elt -> Window Elt
kern0 = amap (+100)

-- kern1 arr = 
--  let sum = afold (+) 0 arr in
--  array (bounds arr) []

#ifdef CARRAY
kern2 :: Window Elt -> Window Elt
kern2 arr = dft arr
#endif

-- type ElT = Float
type Elt = Complex Double

--------------------------------------------------------------------------------
-- Stream Operators

streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap fn instrm = 
    do outstrm <- new
       fork$ loop instrm outstrm
       return outstrm
 where
  loop instrm outstrm = 
   do 
      ilst <- get instrm
      case ilst of 
	Null -> put outstrm Null -- End of stream.
	Cons h t -> 
	  do newtl <- new
	     put outstrm (Cons (fn h) newtl)
	     loop t newtl

-- If this loops forever a serial execution of the program can fail.
countupWindowed :: Int -> Int64 -> Par (WindowedStream Elt)
countupWindowed bufsize target = 
   do outstrm <- new
      fork$ loop (0::Int64) outstrm
      return outstrm
 where 
  bufsize' = (fromIntegral bufsize) :: Int64
  loop :: Int64 -> (WindowedStream Elt) -> Par ()
  loop n strm | n >= fromIntegral target = 
	  do print_$ " [countupWindowed] produced "++ show n ++" elements in windows, met target: "++ show target ++"\n"
	     put strm Null
             return ()
  loop n strm = 
    do let arr = array (0,bufsize-1) [(i, fromIntegral (n + fromIntegral i)) | i <- [0..bufsize-1]]
	       :: Window Elt
       newtl <- new
       put strm (Cons arr newtl)
-- yield? so that we can execute concurrently on a single thread?
       loop (n+bufsize') newtl


-- Count received stream elements and print a count every second:
measureRate :: Stream a -> Par ()
measureRate strm = 
  do 
--     t0 <- _unsafe_dupable getCurrentTime
--     t0 <- _unsafe_dupable getCPUTime
     t0 <- _unsafe_io getTime
     print_$ " [measureRate] Counting stream rate starting at time: "++ show t0

     t0 <- _unsafe_io getTime
     print_$ " [measureRate] Counting stream rate starting at time: "++ show t0

     loop t0 (0::Int64) strm
 where 
  loop time n strm = 
    do x <- get strm 
       case x of 
	 Null -> 
          do print_$ " [measureRate] Hit end of stream after "++show n++" elements."
             return ()
	 Cons hd tl -> 
	  do 
             time2 <- _unsafe_io getTime

--	     if time2 - time > one_second then do
--	     if time2 - time > (one_second `quot` 20) then do
	     if time2 /= time then do
               (print_$ " [measureRate] counting element "++show (n+1) ++ " at time "++ show time2)
--               print_ (show (n+1))
	       loop time2 (n+1) tl
	      else do
               loop time (n+1) tl


#if 0
measureRate :: Stream a -> IO ()
measureRate strm = 
  do 
     t0 <- getTime
     print_$ " [measureRate] Counting stream rate starting at time: "++ show t0
     loop t0 (0::Int64) strm
 where 
  loop time n strm = 
    do x <- get strm 
       case x of 
	 Null -> 
          do print_$ " [measureRate] Hit end of stream after "++show n++" elements."
             return ()
	 Cons hd tl -> 
	  do 
--             time2 <- _unsafe_dupable getTime
             time2 <- _unsafe_io getTime
--	     when (time2 /= time)
             (print_$ " [measureRate] counting element "++show (n+1) ++ " at time "++ show time2)

--	     if time2 - time > one_second then do
--	     if time2 - time > (one_second `quot` 20) then do
	     if time2 /= time then do
               (print_$ " [measureRate] counting element "++show (n+1) ++ " at time "++ show time2)
--               print_ (show (n+1))
	       loop time2 (n+1) tl
	      else do
               loop time (n+1) tl
#endif



sample strm =
  do 
     Cons h1 t1 <- get strm
     print_$ "1st: "++show h1 

     Cons h2 t2 <- get t1
     print_$ "\n  2nd: "++show h2

     Cons h3 t3 <- get t2
     print_$ "\n  3rd: "++show h3
     return ()

--------------------------------------------------------------------------------
  
test = runPar$ 
  do 
--     strm1 <- countup maxBound
     strm1 <- countupWindowed 1024 100000000
     sample strm1

     -- print_$ "\n Next, applying filter... "
     -- strm2 <- streamMap kern2 strm1 

     -- print_$ "\n Resulting stream: "
     -- sample strm2

     measureRate strm1

     print_$ "\n Done"


instance NFData (U.UArray a b) where 
  rnf !arr = ()

instance NFData (C.CArray a b) where 
  rnf !arr = ()

--------------------------------------------------------------------------------
-- Main script

main = do
  putStrLn$ "numCapabilities: "++ show numCapabilities
  putStrLn$ "Frequency in measurable ticks:  "++ show one_second
  print test


--------------------------------------------------------------------------------
-- Helpers and Scrap:

print_ msg = trace msg $ return ()

_unsafe_io :: IO a -> Par a
_unsafe_io io =  let x = unsafePerformIO io in
		 x `seq` return x

_unsafe_dupable :: IO a -> Par a
_unsafe_dupable io = 
  let x = unsafeDupablePerformIO io in 
  x `seq` return x

-- This version simply busy-waits to stay on the same core:
measure_freq2 :: IO Word64
measure_freq2 = do 
  let second = 1000 * 1000 * 1000 * 1000 -- picoseconds are annoying
  t1 <- rdtsc 
  start <- getCPUTime
  let loop !n !last = 
       do t2 <- rdtsc 
	  when (t2 < last) $
	       putStrLn$ "WARNING, measure_freq2: COUNTERS WRAPPED "++ show (last,t2) 
	  cput <- getCPUTime		
	  if (cput - start < second) 
	   then loop (n+1) t2
	   else return (n,t2)
  (n,t2) <- loop 0 t1
  putStrLn$ "  Approx getCPUTime calls per second: "++ commaint n
  when (t2 < t1) $ 
    putStrLn$ "WARNING: rdtsc not monotonically increasing, first "++show t1++" then "++show t2++" on the same OS thread"

  return$ fromIntegral (t2 - t1)

commaint :: Integral a => a -> String
commaint n = 
   reverse $
   concat $
   intersperse "," $ 
   chunk 3 $ 
   reverse (show n)


-- getTime = getCPUTime
-- one_second = 1000000000000 -- picoseconds
getTime = rdtsc
one_second = unsafePerformIO$ measure_freq2


-- work pop 1 peek N push 1 
-- float->float filter 
-- firFilter n coefs = 
-- {

--     float sum = 0;
--     for (int i = 0; i < N; i++)
--       sum += peek(i) * COEFF[N-1-i];
--     pop();
--     push(sum);
--   }
-- }


{-
 Notes:

  [2011.03.19] Right now I'm seeing a weird behavior where when run
  -threaded it prints many of the messages before "new"' in the filter
  stage, but it doesn't print the messages after "put" in that same
  stage.

  This system allows self-stealing, correct?  
  

 -}