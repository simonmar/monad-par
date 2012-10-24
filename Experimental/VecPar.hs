
{-# LANGUAGE MultiParamTypeClasses #-}

module VecPar
       (
         vecFork, getTheVec, initTheVec, 
         liftST
       )
       where

import Control.Monad
import Control.Monad.Par (Par,IVar)
import qualified Control.Monad.Par.Class as PC
-- import Control.Monad.Trans
-- import qualified Control.Monad.Trans.State.Strict as S
-- import qualified Control.Monad.Trans.State.Lazy as SL
import Control.Monad.ST
import Data.STRef

newtype ParVec s a = ParVec ()

getTheVec  = undefined
initTheVec = undefined
forkPartition = undefined


liftST :: ST s a -> ParVec s a
liftST = undefined


vecFork = undefined

instance Monad (ParVec s) where
  
instance PC.ParFuture IVar (ParVec s) where

instance PC.ParIVar IVar (ParVec s) where


--------------------------------------------------------------------------------

p1 :: ST s String
p1 = do
  r <- newSTRef "hi"

  writeSTRef r "hello"

  readSTRef r

