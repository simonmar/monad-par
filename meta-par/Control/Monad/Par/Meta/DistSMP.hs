{-# LANGUAGE CPP #-}

-- | Combines multi-process with multi-thread execution.

module Control.Monad.Par.Meta.DistSMP 

#define DIST_SMP
#include "Dist.hs"
