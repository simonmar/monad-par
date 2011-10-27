#!/usr/bin/env runhaskell
{-# LANGUAGE NamedFieldPuns
  #-}

-- This script generates gnuplot plots.
-- Give it a .dat file as input... (or it will try to open results.dat)

module Main where

import Text.PrettyPrint.HughesPJClass
import Text.Regex
import Data.List
import Data.Maybe (mapMaybe)
import Data.Function
import Control.Monad
import System.Process (system)
import System.IO
import System.FilePath 
import System.Environment

import HSH
import Data.Array (listArray, )
import Data.Monoid (mappend, )
import Debug.Trace

import ScriptHelpers

--------------------------------------------------------------------------------
-- Settings

linewidth = "5.0"

-- Schedulers that we don't care to graph right now.
-- This happens BEFORE rename
--scheduler_MASK = [5,6,99,10]
scheduler_MASK = []

-- Ok, gunplot line type 6 is YELLOW... that's not too smart:
line_types = [0..5] ++ [7..]

round_2digits :: Double -> Double
round_2digits n = (fromIntegral $round (n * 100)) / 100

--x11 = terminal Terminal.X11.cons
--x11 = terminal cons
--x11 = terminal Graphics.Gnuplot.Terminal.X11.cons
--x11 = terminal X11.cons

--------------------------------------------------------------------------------
-- Data Types:

-- Here's the schema for the data from my timing tests:
data Entry = Entry { 
  name     :: String,
  variant  :: String,
  sched    :: String,
  threads  :: Int, 
  tmin     :: Double,
  tmed     :: Double,
  tmax     :: Double,
  normfactor :: Double
}
  deriving Show

instance Pretty Entry where
  --pPrint x = pPrint$ show x
  pPrint Entry { name, sched, variant, threads, tmin, tmed, tmax, normfactor } = 
       pPrint ("ENTRY", name, sched, variant, threads, (tmin, tmed, tmax), normfactor )
--       pPrint ("ENTRY", name, variant, sched, threads, tmin, tmed, tmax, normfactor)

--------------------------------------------------------------------------------
-- Sloppy parsing of lines-of-words:

parse [a,b,c,d,e,f] = Just $
  Entry { name     = a, 
	  variant  = "_", -- TODO - phase out
	  sched    = b,
	  threads  = read c,
	  tmin     = read d,
	  tmed     = read e,
	  tmax     = read f,
	  normfactor = 1.0
	}

parse other = 
   trace ("WARNING: Cannot parse, wrong number of fields, "++ show (length other) ++" expected 8 or 9: "++ show other) $ 
   Nothing
   
--------------------------------------------------------------------------------
-- Let's take a particular interpretation of Enum for pairs:
instance (Enum t1, Enum t2) => Enum (t1,t2) where 
  succ (a,b) = (succ a, succ b)
  pred (a,b) = (pred a, pred b)
  toEnum n = (toEnum n, toEnum n)
  fromEnum (a,b) = case (fromEnum a, fromEnum b) of
                     (x,y) | x == y -> x
		     (x,y) -> error$ "fromEnum of pair: nonmatching numbers: " ++ show x ++" and "++ show y


-- Removes single blanks and separates lines into groups based on double blanks.
sepDoubleBlanks :: [String] -> [[String]]
sepDoubleBlanks ls = loop [] ls 
 where 
  loop acc []        = [reverse acc]
  loop acc ("":"":t) = reverse acc : loop [] (stripLeadingBlanks t)
  loop acc ("":t)    = loop acc t
  loop acc (h :t)    = loop (h:acc) t 
  stripLeadingBlanks []     = [] 
  stripLeadingBlanks ("":t) = stripLeadingBlanks t
  stripLeadingBlanks ls     = ls


--------------------------------------------------------------------------------

groupSort fn = 
   (groupBy ((==) `on` fn)) . 
   (sortBy (compare `on` fn))

-- Add three more levels of list nesting to organize the data:
--organize_data :: [Entry] -> [[[[Entry]]]]
organize_data :: [Entry] -> [[[[Entry]]]]
organize_data = 
	 (map (map (groupSort sched)))  . 
  	      (map (groupSort variant)) .
                   (groupSort name)


newtype Mystr = Mystr String

instance Show Mystr where
  show (Mystr s) = s

--               Name, Variant, Scheduler,        Threads, BestTime, Speedup
data Best = Best (String, String, String,   Int, Double, Double)


{-
   I ended up giving up on using the gnuplot package on hackage.

   The below script turns a single benchmark into a gnuplot script
   (produced as a string).

   plot_benchmark2 expects entries with three levels of grouping, from
   outside to in:
     * Name 
     * Variant (e.g. variant of the benchmark)
     * Sched

-}
plot_benchmark2 :: String -> [[[Entry]]] -> IO Best

plot_benchmark2 root entries = 
    do action $ filter goodSched (concat entries)
       return$ Best (benchname, bestvariant, 
		     bestsched, bestthreads, best, basetime / best)
 where 
  benchname = name $ head $ head $ head entries
  -- What was the best single-threaded execution time across variants/schedulers:

  goodSched [] = error "Empty block of data entries..."
  goodSched (h:t) = not $ (sched h) `elem` scheduler_MASK
  
  -- Knock down two levels of grouping leaving only Scheduler:
  cat = concat $ map concat entries

  threads0 = filter ((== 0) . threads) cat
  threads1 = filter ((== 1) . threads) cat

  map_normalized_time = map (\x -> tmed x / normfactor x)

  times0 = map_normalized_time threads0
  times1 = map_normalized_time threads1

  basetime = if    not$ null times0 
	     then foldl1 min times0
	     else if    not$ null times1 
		  then foldl1 min times1
		  else error$ "\nFor benchmark "++ show benchname ++ " could not find either 1-thread or 0-thread run.\n" ++
		              --"ALL entries: "++ show (pPrint cat) ++"\n"
		              "\nALL entries threads: "++ show (map threads cat)

  best = foldl1 min $ map_normalized_time cat
  Just best_index = elemIndex best $ map_normalized_time cat
  bestsched   = sched$ cat !! best_index
  bestvariant = variant$ cat !! best_index
  bestthreads = threads$ cat !! best_index

  (filebase,_) = break (== '.') $ basename benchname 

  -- If all normfactors are the default 1.0 we print a different message:
  --let is_norm = not$ all (== 1.0) $ map normfactor ponits
  norms = map normfactor cat
  default_norms = all (== 1.0) $ norms
  max_norm = foldl1 max norms

  scrub '_' = ' '
  scrub x = x
  -- scrub [] = []
  -- scrub ('_':t) = "\\_"++ scrub t
  -- scrub (h:t)   = h : scrub t

  action lines = 
   do 
      let scriptfile = root ++ filebase ++ ".gp"
      putStrLn$ "  Dumping gnuplot script to: "++ scriptfile

      putStrLn$ "    NORM FACTORS "++ show norms


      runIO$ echo "set terminal postscript enhanced color\n"         -|- appendTo scriptfile
      runIO$ echo ("set output \""++filebase++".eps\"\n")            -|- appendTo scriptfile
      runIO$ echo ("set title \"Benchmark: "++ map scrub filebase ++
		   ", speedup relative to serial time " ++ show (round_2digits $ basetime * max_norm) ++" seconds "++ 
--		   "for input size " ++ show (round_2digits max_norm)
		   (if default_norms then "" else "for input size " ++ show (round max_norm))
		   --if is_norm then "normalized to work unit"
		   --if default_norms then "" else " per unit benchmark input"
		   ++"\"\n") -|- appendTo scriptfile
      runIO$ echo ("set xlabel \"Number of Threads\"\n")             -|- appendTo scriptfile
      runIO$ echo ("set ylabel \"Parallel Speedup\"\n")              -|- appendTo scriptfile


      runIO$ echo ("set xrange [1:]\n")                             -|- appendTo scriptfile
      runIO$ echo ("set key left top\n")                             -|- appendTo scriptfile
      runIO$ echo ("plot \\\n")                                      -|- appendTo scriptfile

      -- In this loop lets do the errorbars:
      forM_ (zip [1..] lines) $ \(i,points) -> do 
          let datfile = root ++ filebase ++ show i ++".dat"
	  runIO$ echo ("   \""++ basename datfile ++"\" using 1:2:3:4 with errorbars lt "++
	              show (line_types !! i)	              
		      ++" title \"\", \\\n") -|- appendTo scriptfile

      -- Now a second loop for the lines themselves and to dump the actual data to the .dat file:
      forM_ (zip [1..] lines) $ \(i,points) -> do 
          let datfile = root ++ filebase ++ show i ++".dat"          
	  let schd = sched$   head points  -- should be the same across all point
	  let var  = variant$ head points  -- should be the same across all point
	  let nickname = var ++"/"++ schd
	  runIO$ echo ("# Data for variant "++ nickname ++"\n") -|- appendTo datfile
          forM_ points $ \x -> do 

              -- Here we print a line of output:
	      runIO$ echo (show (fromIntegral (threads x)) ++" "++
			   show (basetime / (tmed x / normfactor x))        ++" "++
                           show (basetime / (tmax x / normfactor x))        ++" "++ 
			   show (basetime / (tmin x / normfactor x))        ++" \n") -|- appendTo datfile

	  let comma = if i == length lines then "" else ",\\"
	  runIO$ echo ("   \""++ basename datfile ++
		       "\" using 1:2 with lines linewidth "++linewidth++" lt "++ 
		       show (line_types !! i) ++" title \""++nickname++"\" "++comma++"\n")
		   -|- appendTo scriptfile

      --putStrLn$ "Finally, running gnuplot..."
      --runIO$ "(cd "++root++"; gnuplot "++basename scriptfile++")"
      --runIO$ "(cd "++root++"; ps2pdf "++ filebase ++".eps )"



isMatch rg str = case matchRegex rg str of { Nothing -> False; _ -> True }

main = do 
 args <- getArgs 
 let file = case args of 
	      [f] -> f 
	      []     -> "results.dat"
 dat <- run$ catFrom [file] -|- remComments 

 -- Here we remove
 let parsed = mapMaybe (parse . filter (not . (== "")) . splitRegex (mkRegex "[ \t]+")) 
	          (filter (not . isMatch (mkRegex "ERR")) $
		   filter (not . isMatch (mkRegex "TIMEOUT")) $
		   filter (not . null) dat)
 let organized = organize_data parsed

 putStrLn$ "Parsed "++show (length parsed)++" lines containing data."
-- This can get big, I was just printing it for debugging:
-- print organized

 let root = "./" ++ dropExtension file ++ "_graphs/"
 -- For hygiene, completely anhilate output directory:
 system$ "rm -rf "  ++root ++"/"
 system$ "mkdir -p "++root
 bests <- 
  forM organized    $ \ perbenchmark -> do 
   best <- plot_benchmark2 root perbenchmark
   forM_ perbenchmark $ \ pervariant -> 
    forM_ pervariant   $ \ persched -> 
      do let mins = map tmin persched
 	 let pairs = (zip (map (fromIntegral . threads) persched) mins)
	 --putStrLn$ show pairs
	 --plot Graphics.Gnuplot.Terminal.X11.cons (path pairs)
	 --System.exitWith ExitSuccess
	 --plot x11 (path pairs)
         return ()

   return best

 putStrLn$ "Now generating final plot files...\n\n"

 let summarize hnd = do 
       hPutStrLn hnd $ "# Benchmark, Variant, Scheduler, best #threads, best median time, max parallel speedup: "
       hPutStrLn hnd $ "# Summary for " ++ file

       let pads n s = take (n - length s) $ repeat ' '
       let pad  n x = " " ++ (pads n (show x))

       forM_ bests $ \ (Best(name, variant, sched, threads, best, speed)) ->
	 hPutStrLn hnd$ "    "++ name++  (pad 25 name) ++
			  variant++ (pad 10 variant)++
			  sched++   (pad 10 sched) ++
			  show threads++ (pad 5 threads)++ 
			  show best ++   (pad 15 best) ++
			  show speed 
       hPutStrLn hnd$ "\n\n"

 putStrLn$ "Done."
 summarize stdout
 withFile (dropExtension file `addExtension` "summary") WriteMode $ summarize 

