

-- import Control.Monad.IO.Class
-- import Control.Monad.Par.IO
-- import Control.Monad.Par.Class

-- import Control.Monad.Par.Scheds.Trace
import Control.Monad.Par.Scheds.Direct

import System.IO.Unsafe(unsafePerformIO)
import System.IO


foo :: Par Int
foo = do fork $ loop 0
         return 3

loop :: Integer -> Par ()
loop n = do case unsafePerformIO (dot n) of
             () -> return ()
            loop (n+1)

dot :: Integer -> IO ()
dot n | n `mod` 10000 == 0 =
  do putStr "."
     hFlush stdout
dot _ = return ()

main :: IO ()
main = do
  putStrLn "Launching runPar:"
  x <- runParIO foo
  putStrLn "Returned from runPar:"  
  print x
  