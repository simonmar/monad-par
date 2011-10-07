{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances
  #-}
{-| 

UNFINISHED 

    This module provides a basic implementation of channels that is
    usable with any monad satisfying ParIVar.
 -}

module Control.Monad.Par.Chan where 

import Control.Monad.Par.Class

-- type SendPort v a = ()
-- type RecvPort v a = ()
data SendPort a = SendPort a
data RecvPort a = RecvPort a

-- instance ParIVar m v => ParChan m (SendPort v) (RecvPort v) where 
instance ParIVar m v => ParChan m SendPort RecvPort where 
  newChan = undefined
  send    = undefined
  recv    = undefined
