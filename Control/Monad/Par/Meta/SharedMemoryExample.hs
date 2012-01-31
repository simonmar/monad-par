module Main where

import Control.Applicative
import GHC.Conc (numCapabilities)

import Control.Monad.Par.Meta
import Control.Monad.Par.Meta.SharedMemoryOnly

parfib :: Int -> Par Int
parfib 0 = return 0
parfib 1 = return 1
parfib n = do
  iv <- spawn (parfib (n-1))
  (+) <$> get iv <*> parfib (n-2)

main :: IO ()
main = do print numCapabilities; print . runPar $ parfib 38