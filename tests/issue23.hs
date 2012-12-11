import Control.Monad.Par

-- OR, with the Trace scheduler:
-- import Control.Monad.Par.Scheds.Trace
-- import Control.Monad.Par.Combinator

test :: [Int] -> IO [Int]
test xs = do
    let list = runPar $ parMap (\x -> x + 1) xs
    putStrLn $ show list
    test list

main = do
    test [1]
