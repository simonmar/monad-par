

import Control.Monad.Par
import Control.Monad.Par.OpenList
import Control.DeepSeq
import Data.Array.Unboxed


bufsize = 256

--type Stream a = OpenList (UArray Int a)
type Stream a = IList (UArray Int a)

stage :: Stream Float -> Stream Float -> Par ()
stage instrm outstrm = 
--  undefined
  do 
     let input = hd instrm
         output = amap (+1) input
     cell <- newCell output
     -- Append to the end of the output stream:
     put (tl outstrm) cell
     nxt <- get (tl instrm)
     stage nxt cell

source :: Stream Float -> Par ()
source outstrm = loop 0 outstrm
 where 
  loop n strm = 
    do let arr = array (0,n-1) [(i,fromIntegral (n+i)) | i <- [0..bufsize]]
       cell <- newCell arr
       put (tl strm) cell 
       loop (n+bufsize) cell


test = 
  do cell <- newCell (array (0,0) [])
     source cell

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

