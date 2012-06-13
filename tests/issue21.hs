{-

$ cabal install -O2 monad-par-0.3
$ ghc -O2 -threaded -rtsopts -with-rtsopts -N issue21.hs
$ ./issue21 10000000
2089877
$ ./issue21 10000000
issue21: <<loop>>issue21: issue21: issue21: thread blocked indefinitely in an MVar operation
<<loop>>

<<loop>>
issue21: <<loop>>
$ ghc -V
The Glorious Glasgow Haskell Compilation System, version 7.4.1

-}

import Control.Monad.Par
import System.Environment (getArgs)
import Control.DeepSeq

data M = M !Integer !Integer !Integer !Integer
instance NFData M

instance Num M where
  m * n = runPar $ do
    m' <- spawn (return m)
    n' <- spawn (return n)
    m'' <- get m'
    n'' <- get n'
    return (m'' `mul` n'')

(M a b c d) `mul` (M x y z w) = M
  (a * x + b * z) (a * y + b * w)
  (c * x + d * z) (c * y + d * w)

fib :: Integer -> Integer
fib n = let M f _ _ _ = M 0 1 1 1 ^ (n + 1) in f

main :: IO ()
main = print . length . show . fib . read . head =<< getArgs

