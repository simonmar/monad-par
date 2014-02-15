
-- | Use a RNG transformer on top of LVish.

module LVishPlusRNG
       (
         runPar,
         module Control.Par.Class 
       ) where

import qualified Control.LVish as L 
import Control.Par.Class
import Control.Par.RngT

-- UNFINISHED:
runPar = 
