


-- #include "flops.hs"

-- parMapReduceRange :: NFData a => InclusiveRange -> (Int -> Par a) -> (a -> a -> Par a) -> a -> Par a
import Control.Monad.Par
import Data.Int
import System.Environment


main = do 
          putStrLn "Program to sum 2^N ones with 2^N parallel tasks.  (Similar to parFib.)"
          args <- getArgs	  
          let exponent = 
                   case args of 
		      []  -> 2
		      [n] -> read n
		      _  -> error "wrong number of arguments"

              sum = runPar $ 
		    parMapReduceRange (InclusiveRange 1 (2^exponent))
				      (\_ -> return 1) 
				      (\ x y -> return (x+y))
				      0
          putStrLn$ "Resulting Sum for 2^"++show exponent++": " ++ show (sum :: Int64)

