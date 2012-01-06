

import qualified Data.List as L

import qualified Data.Vector.Algorithms.Intro as VA

-- Haskell standard library
sort_list1 l = L.sort l


-- vector-algorithms package which uses a similar algorithm as the C++ STL sort:
sort_vector2 v = undefined


-- main = do args <- getArgs
--           let (t, size) = case args of
--                             [] -> (2, 18)
--                             [t] -> ((read t), 18)
--                             [t, n] -> ((read t), (read n))

--           g <- getStdGen
--           let rands = genRandoms size g

--           putStrLn $ "Merge sorting " ++ show (V.length rands) ++ 
--                      " elements. First deepseq the rands."
--           --evaluate (deepseq rands ())


--           putStrLn "Monad-par based version:"
--           print $ take 8 $ V.toList $ runPar $ mergesort t rands

