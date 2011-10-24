{-# OPTIONS_GHC -fwarn-unused-imports #-}

module Main(main) where

-- TEMP: Fixing the trace scheduler because of its runParAsync support:
import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Scheds.TraceInternal (runParAsync)
import Control.Monad.Par.Stream as S
import Control.Monad.Par.Logging
import Control.Exception
import Data.Complex
import GHC.Conc as Conc
import Debug.Trace
import Math.FFT (dft)

type Elt = Complex Double

fft_kern :: Window Elt -> Window Elt
fft_kern arr = dft arr

--   -- TEMP, evaluate one element to make sure the fft really gets called:
-- --  trace ("One elt sample: "++ show (arr!10)) $
--   case arr2 ! 10 of _ -> arr2
--  where arr2 = dft arr

--------------------------------------------------------------------------------
-- Main script

-- target  = maxBound
target  = 10 * 1000 * 1000
bufsize = 1024

main = do
  putStrLn$ "numCapabilities: "++ show numCapabilities
  putStrLn$ "  Frequency in measurable ticks:  "++ commaint oneSecond ++ "\n"


  putStrLn$ "Performing FFT of "++ commaint target ++" numbers windowed into buffers of size "++ show bufsize ++"\n"

  results <- evaluate $ runParAsync$ do 

       strm1 <- countupWin bufsize target :: Par (WStream Elt)

       print_$ "\n Next, applying FFT filter... "
       strm2 <- streamMap fft_kern strm1 

-- Make a pipeline of 10 stages:
--       strm2 <- foldl (\ s _ -> streamMap kern0) strm1 [1..10]

       print_$ "\n Stream graph constructed, returning from Par computation... "
       return strm2

  measureRate results
  putStrLn$ "End of stream reached.  All done."


print_ msg = trace msg $ return ()

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



 -}
