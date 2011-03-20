{-# LANGUAGE BangPatterns, CPP #-}

module Main(main) where

#define FFTW
#define CARRAY

import Control.Monad
import Control.Monad.Par as P
import Control.Monad.Par.OpenList
import Control.DeepSeq
import Control.Exception

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
import GHC.Conc as Conc
import GHC.IO (unsafePerformIO, unsafeDupablePerformIO, unsafeInterleaveIO)
import Debug.Trace
#ifdef FFTW
import Math.FFT (dft)
type Elt = Complex Double
#else
type Elt = Float
#endif

type Stream a = IVar (IList a)

-- A windowed stream passes chunks of stream elements.
type WindowedStream a = Stream (Window a)

#ifdef CARRAY
type Window a = CArray Int a
#else
type Window a = U.UArray Int a
#endif

kern0 :: Window Elt -> Window Elt
kern0 = amap (+100)


-- UNFINISHED
-- Do a pointless quadratic operation:
kern1 arr = 
-- let sum = afold (+) 0 arr in
 undefined
-- array (bounds arr) []

-- UNFINISHED
afoldRange fn zer (InclusiveRange st end) arr = 
  undefined
 

#if defined(CARRAY) && defined(FFTW)
kern2 :: Window Elt -> Window Elt
kern2 arr = 
  -- TEMP, evaluate one element to make sure the fft really gets called:
--  trace ("One elt sample: "++ show (arr!10)) $
  case arr2 ! 10 of _ -> arr2
 where arr2 = dft arr
#endif


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
	  do print_$ " [countupWindowed] Done.  Produced "++ show n ++" elements in windows, met target: "++ show target ++"\n"
	     put strm Null
             return ()
  loop n strm = 
    do let arr = array (0,bufsize-1)
                       [(i, fromIntegral (n + fromIntegral i)) | i <- [0..bufsize-1]]
	       :: Window Elt
       newtl <- new
       put strm (Cons arr newtl)
       P.yield  -- This is necessary to execute properly when there
		-- aren't enough worker threads to go around.
       loop (n+bufsize') newtl

-- Convert a stream to a lazy list.  Spin wait (with yield) until stream elements are available.
streamToListSpin :: Stream a -> IO [a]
streamToListSpin strm = 
   do x <- pollIVar strm
      case x of
        Nothing  -> do Conc.yield            -- run other GHC threads
                       streamToListSpin strm -- spin wait
	Just (ils) -> 
	    case ils of 
	      Null     -> return []
	      Cons h t -> return (h : unsafePerformIO (unsafeInterleaveIO (streamToListSpin t)))


-- TODO: If it is unavailable we should help run the computation and then try again.
-- This version will do runParAsync itself:
-- Run a Par computation to produce a stream.  Convert that stream to a lazy list.
-- streamToList2 :: Par (Stream a) -> IO [a]
-- streamToList2 parcomp = loop (runParAsync parcomp) 
--  where 
--   loop strm = 
--    do x <- pollIVar strm
--       case x of
--         Nothing  -> 
-- 	    -- For now we just spin:
-- 	    do yield -- run other GHC threads
-- 	       loop strm
-- 	Just (ils) -> 
-- 	    case ils of 
-- 	      Null     -> return []
-- 	      Cons h t -> return (h : unsafePerformIO (unsafeInterleaveIO (loop t)))

measureRate :: Stream a -> IO ()
measureRate strm = 
  do 
     t0 <- getTime
     print_$ " [measureRate] Counting stream rate starting at time: "++ show t0
     lazyls <- streamToListSpin strm
     loop t0 t0 (0::Int64) (0::Int64) lazyls
     
 where 
  loop start time lastN n [] = 
    do print_$ " [measureRate] Hit end of stream after "++show n++" elements."
       return ()

  loop start time lastN n (h:t) = 
       do 
	  time2 <- getTime
	  if time2 - time > one_second then do
	    (print_$ " [measureRate] current rate: "++show (n+1-lastN) ++ 
	             "  Total elem/time "++ commaint (n+1)++ " / " ++commaint (time2-start))
--               print_ (show (n+1))
	    loop start time2 (n+1) (n+1) t
	   else do
	    loop start time  lastN (n+1) t




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

instance NFData (U.UArray a b) where 
  rnf !arr = ()

instance NFData (C.CArray a b) where 
  rnf !arr = ()

--------------------------------------------------------------------------------
-- Main script

main = do
  putStrLn$ "numCapabilities: "++ show numCapabilities
  putStrLn$ "  Frequency in measurable ticks:  "++ commaint one_second ++ "\n"

  results <- evaluate $ runParAsync$ do 
  --     strm1 <- countup maxBound
       strm1 <- countupWindowed 1024 maxBound
--       strm1 <- countupWindowed 8 10000
       -- sample strm1

       print_$ "\n Next, applying filter... "
--       let strm2 = strm1
--       strm2 <- streamMap kern0 strm1 
       strm2 <- streamMap kern2 strm1 

-- Make a pipeline of 10 stages:
--       strm2 <- foldl (\ s _ -> streamMap kern0) strm1 [1..10]

       -- print_$ "\n Resulting stream: "
       -- sample strm2

       return strm2

  measureRate results
  putStrLn$ "Done"


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
  
  [2011.03.19] 
   Quick measurements on wasp:
   ---------------------------
    Throughput of just countupWindowed + measureRate ...
      2 threads is 8600 * 1024 = 8,806,400 = 8.8mHz.

    * With -N4 it uses ~234% CPU and gets sligthly confused, lowering
      throughput to 5800-6100 windows/sec from 8600

    * It actually does work (e.g. doesn't deadlock) to run it with -N1
      because there's only one kernel.  The throughput is much more
      variable.  From 4800-8500 win/sec.  GC time is tiny in this
      case, 0.5%.
   ---------------------------
    Throughput of a single amap (+100) kernel = 

   * Gets stuck waiting for first kernel -N1 as expected (and fills memory).

   * on -N2 it gets a rate of between 7937 and 8200 wins/sec but then
     it gets stuck after about 10 seconds.  I don't fully understand
     this.  The two threads should be enough for the two kernels
     right?  Currently Par.hs forks a "replacement thread" to continue
     working on the queue when runParAsync returns.  Therefore there
     should be enough haskell threads, even if there are only two Par
     scheduler threads.  Those two par workers should be preemptable by
     the measureRate thread....

   * on -N3 it works, and gives 6200-7100 wins/sec throughput.  Uses
     ~245% CPU.  Presumably the two par worker threads are hammering
     away and the measureRate one is less busy.  But if I leave it
     running for a while it grew up to 79% (of 16gb) mem usage.
     Strange, -s claims the following.  How can max resdency be so low!?

	 2,993,287,338,808 bytes allocated in the heap
	   17,134,304,736 bytes copied during GC
		3,744,408 bytes maximum residency (152145 sample(s))
		1,870,240 bytes maximum slop
		       14 MB total memory in use (3 MB lost due to fragmentation)

	   Generation 0: 4961153 collections, 4961152 parallel, 696.02s, 63.29s elapsed
	   Generation 1: 152145 collections, 152145 parallel, 115.00s, 22.66s elapsed

	   Parallel GC work balance: 1.08 (2102121164 / 1946757116, ideal 3)

				 MUT time (elapsed)       GC time  (elapsed)
	   Task  0 (worker) :    0.02s    (  0.00s)       0.00s    (  0.00s)
	   Task  1 (worker) :  1927.40s    (742.55s)       0.00s    (  0.00s)
	   Task  2 (worker) :  1927.40s    (742.55s)       0.00s    (  0.00s)
	   Task  3 (worker) :  1928.34s    (743.40s)       0.00s    (  0.00s)
	   Task  4 (worker) :  1928.34s    (743.40s)       0.00s    (  0.00s)
	   Task  5 (bound)  :    0.00s    (  0.00s)       0.11s    (  0.02s)
	   Task  6 (worker) :  1928.34s    (743.40s)       0.00s    (  0.00s)
	   Task  7 (worker) :  1117.43s    (743.40s)     810.91s    ( 85.93s)

	   SPARKS: 0 (0 converted, 0 pruned)

	   INIT  time    0.00s  (  0.00s elapsed)
	   MUT   time  1117.32s  (743.40s elapsed)
	   GC    time  811.02s  ( 85.95s elapsed)
	   EXIT  time    0.00s  (  0.01s elapsed)
	   Total time  1928.34s  (829.36s elapsed)

	   %GC time      42.1%  (10.4% elapsed)

	   Alloc rate    2,678,988,417 bytes per MUT second

	   Productivity  57.9% of total user, 134.7% of total elapsed

	 gc_alloc_block_sync: 6092257
	 whitehole_spin: 0
	 gen[0].sync_large_objects: 190255
	 gen[1].sync_large_objects: 267

   Oh, maybe because of the CArray's all the real storage is outside haskell's heap.

   There must be a memory leak in streamMap.  Trying to fix it:

     (1) Factored out 'loop'.  I need to try to ensure that no closure
         holds onto the original head of the stream.  Wow!  That
         lowered throughput a lot (-N3) and drove cpu usage up! 3500
         wins/sec declining to 300.  And it still leaks.
           The key difference seems to be passing the extra "fn" argument to loop.

     (2) Hmm... I went back to what I *thought* was the previous form
         above (that leaked).  But now it's getting the good >6000
         throughput and doesn't seem to be leaking.  It gives memory
         back to the system and goes up and down in mem footprint.
         But now it uses 300% cpu.  The only difference I can see is
         that I changed the module export decl.  How could this matter
         if compiling into an executable?  Nevertheless maybe this
         helps it inline....  Now I can run it for 10 min with minimal
         memory usage.
         
         -qa seems to help the variance on -N4, i.e. with more workers
          than kernels.

   ---------------------------
    Throughput of a single FFT kernel.

    * Oops, maybe this accounts for the difference above between
      leaking/non-leaking.  The FFT version maintains a high >7000
      wins/sec throughput.  But it leaks memory.  Maybe it's not
      really doing the FFT and is leaking suspensions?

    * Nope... I tried forcing the FFT by inspecting one element of the
      output.  Still leaks.  Well, the answer is simple.  It just
      can't keep up with a source that has no backpressure.  To
      confirm this hypothesis, I ran it with -N1 (with the new
      yielding source operator).  NOPE, it still leaks.



 -}
