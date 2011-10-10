{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE PackageImports #-}

module Main(main) where

import Control.Monad.Par 
import Control.Monad.Par.Scheds.TraceInternal (runParAsync)
import Control.Monad.Par.Stream as S

--------------------------------------------------------------------------------
-- Main script

main = do

  -- Generate 20 million elements:
  let s = countupWin 1024 (20 * 1000 * 1000) :: Par (WStream Int)
  measureRate $ runParAsync s
   
  putStrLn$ "Done with 5 million elements."
