

import Control.Monad.Par
import Control.Monad.Par.OpenList
import Control.DeepSeq
import Data.Array.Unboxed
import Debug.Trace


-- bufsize = 256
bufsize = 8

--type Stream a = OpenList (UArray Int a)
type Stream a = IVar (IList (UArray Int a))

stage :: Stream Float -> Stream Float -> Par ()
stage instrm outstrm = 
  do ilst <- get instrm
     case ilst of 
       Null -> put outstrm Null
       Cons h t -> 
	 do let output = amap (+100) h
	    print_$ "Executing filter stage, output: "++ show output
	    newtl <- new
	    -- Append to the end of the output stream:
	    put outstrm (Cons output newtl)
	    print_ "Put result to output stream..."
	    stage t newtl

-- If this loops forever a serial execution of the program can fail.
source :: Int -> Stream Float -> Par ()
source max outstrm = loop 0 outstrm
 where 
  loop n strm | n > max = do put strm Null
                             return ()
  loop n strm = 
    do let arr = array (0,bufsize-1) [(i,fromIntegral (n+i)) | i <- [0..bufsize-1]]
       newtl <- new
       put strm (Cons arr newtl)
-- yield?
       loop (n+bufsize) newtl

-- print_ msg = trace msg (return ())

sample strm =
  do 
     Cons h1 t1 <- get strm
     print_$ "1st: "++show h1 

     Cons h2 t2 <- get t1
     print_$ "\n  2nd: "++show h2

     Cons h3 t3 <- get t2
     print_$ "\n  3rd: "++show h3
     return ()

--length = 
  


test = runPar$ 
  do strm1 <- new
--     fork$ source 10000 strm1
     fork$ source 100 strm1
--     fork$ source 5 strm1
     sample strm1

     print_$ "\n Next, applying filter... "
     strm2 <- new
     stage strm1 strm2

     print_$ "\n Resulting stream: "
     sample strm2

     print_$ "\n Done"


print_ msg = trace msg $ return ()

instance NFData (UArray a b) where 

main = do
  putStrLn "Hello"
  print test


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

