

import Control.Monad.Par
import Control.Monad.Par.OpenList
import Control.DeepSeq
import Data.Array.Unboxed
import Debug.Trace


-- bufsize = 256
bufsize = 8

--type Stream a = OpenList (UArray Int a)
type Stream a = IList (UArray Int a)

stage :: Stream Float -> Stream Float -> Par ()
stage instrm outstrm = 
  do 
     let input = hd instrm
         output = amap (+100) input
     print_$ "Executing filter stage, output: "++ show output
     cell <- newCell output
     -- Append to the end of the output stream:
     put (tl outstrm) cell
     nxt <- get (tl instrm)
     print_ "Put result to output stream..."
     stage nxt cell

-- If this loops forever a serial execution of the program can fail.
source :: Int -> Stream Float -> Par ()
source max outstrm = loop 0 outstrm
 where 
  loop n strm | n > max = do put (tl strm) Null
                             return ()
  loop n strm = 
    do let arr = array (0,bufsize-1) [(i,fromIntegral (n+i)) | i <- [0..bufsize-1]]
       cell <- newCell arr
       put (tl strm) cell 
       loop (n+bufsize) cell

-- print_ msg = trace msg (return ())

sample cell1 =
  do 
     print_$ "1st: "++show (hd cell1)
     cell2 <- get (tl cell1)
     print_$ "\n  2nd: "++show (hd cell2)
     cell3 <- get (tl cell2)
     print_$ "\n  3rd: "++show (hd cell3)



test = runPar$ 
  do strm1 <- newCell (array (0,0) [])
--     fork$ source 10000 strm1
     fork$ source 100 strm1
--     fork$ source 5 strm1
     sample strm1

     print_$ "\n Next, applying filter... "
     strm2 <- newCell (array (0,0) [])
     stage strm1 strm2

     print_$ "\n Resulting stream: "
     sample strm2

     print_$ "\n Done"


print_ msg = trace msg $ return ()

instance NFData (UArray a b) where 

main = do
  putStrLn "Hello"



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

