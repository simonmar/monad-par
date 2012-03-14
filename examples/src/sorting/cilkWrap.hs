{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types
import System.Environment (getArgs)
import System.Random
import Data.List
import System.CPUTime (getCPUTime)
import Text.Printf

type ElmT  = Word32
type CElmT = CUInt

foreign import ccall unsafe "wrap_cilksort"
  c_cilksort ::  Ptr CElmT -> Ptr CElmT -> CLong -> IO CLong

foreign import ccall unsafe "run_cilksort"
  c_run_cilksort :: CLong -> IO CLong 

main :: IO ()
main = 
  do args <- getArgs 
     let (size,runs) = case args of
           []    -> (300000, 1)
           [m]   -> (read m, 1)
           [m,n] -> (read m, read n)
                
     printf "Running cilksort with a size of %d (foreign array) \
        \ (%d runs)\n" (fromIntegral size :: Int) (runs :: Int)

     runCilkSort size runs

     printf "Running cilksort with a size of %d (Haskell array): \ 
                \ (%d runs).\n" (fromIntegral size :: Int) (runs :: Int)

     -- TODO: would like to do this
     -- runCilkSort' size runs

     seed <- newStdGen
     let a = randomList (fromIntegral size) seed
         b = randomList (fromIntegral size) seed
       in 
        withArray a $ \pa ->
        withArray b $ \pb ->
        runCilkSort' (castPtr pa) (castPtr pb) size runs

runCilkSort :: CLong -> Int -> IO ()
runCilkSort _ 0 = return ()
runCilkSort  sz n = do
  ticks <- c_run_cilksort sz
  putStrLn $ "ran in " ++ show ticks ++ " ticks"
  runCilkSort sz (n - 1)

runCilkSort' :: Ptr CElmT -> Ptr CElmT -> CLong -> Int -> IO ()
runCilkSort' _ _ _ 0 = return ()
runCilkSort' xs ys sz n = do
  ticks <- c_cilksort xs ys sz
  putStrLn $ "ran in " ++ show ticks ++ " ticks"
  runCilkSort' xs ys sz (n - 1)

randomList :: Int -> StdGen -> [ElmT]
randomList n = take n . unfoldr (Just . random)

