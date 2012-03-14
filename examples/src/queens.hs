{-# LANGUAGE BangPatterns, CPP #-}
import System.Environment
import Control.Monad
import Control.Seq
import qualified Control.Monad.Par.Combinator as C
#ifdef PARSCHED
import PARSCHED
#else
import Control.Monad.Par
#endif

nqueens :: Int -> Par [[Int]]
nqueens nq = step 0 []
  where
    threshold = 5

    step :: Int -> [Int] -> Par [[Int]]
    step !n b
       | n >= threshold = return (iterate gen [b] !! (nq - n))
       | otherwise = do
          rs <- C.parMapM (step (n+1)) (gen [b])
          return (concat rs)

    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: [[Int]] -> [[Int]]
    gen bs = [ (q:b) | b <- bs, q <- [1..nq], safe q 1 b ]

nqueensNested :: Int -> [[Int]]
nqueensNested nq = step 0 []
  where
    threshold = 5

    step :: Int -> [Int] -> [[Int]]
    step !n b
       | n >= threshold = iterate gen [b] !! (nq - n)
       | otherwise = 
          let rs = runPar $ C.parMap (step (n+1)) (gen [b])
          in concat rs

    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: [[Int]] -> [[Int]]
    gen bs = [ (q:b) | b <- bs, q <- [1..nq], safe q 1 b ]


main = do
    args <- getArgs	  
    let (version,n) = case args of 
            []    -> ("monad",8)
            [v,n] -> (v,read n)

    case version of 
        "nested" -> do 
                putStrLn "Monad-par nested version:"
                print (length (nqueensNested n))
        "monad"  -> do 
                putStrLn "Monad-par based version:"
                print (length (runPar (nqueens n)))
        _        -> error$ "unknown version: "++version

