
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module VecPar
       -- (
       --   forkWithVec, getTheVec, initTheVec, 
       --   liftST,

       --   p1
       -- )
       where

import Control.Monad
import Control.Monad.Par.IO (ParIO,IVar)
import qualified Control.Monad.Par.Class as PC
-- import Control.Monad.Trans
-- import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.ST
import Data.STRef
import Data.Vector.Mutable
-- import GHC.IO (unsafeSTToIO)
import Control.Monad.Trans (lift)

import Prelude hiding (read, length)

newtype ParVec s elt a = ParVec ((S.StateT (STVector s elt) ParIO) a)
 deriving Monad


getTheVec :: ParVec s elt (STVector s elt)
getTheVec  = undefined

initTheVec :: Int -> ParVec s elt ()
initTheVec = undefined

forkWithVec :: Int
            -> (forall sleft  . ParVec sleft elt a)
            -> (forall sright . ParVec sright elt b)
            -> ParVec s elt (a,b)
forkWithVec mid (ParVec lef) (ParVec rig) = ParVec $ do
  v <- S.get
  let a = slice 0 mid v
      b = slice mid (length v - mid) v
  S.put a
  lef
  -- TODO: Move at least lef, and possibly lef & rig into the following 'lift':
  lift ((do PC.spawn_ (return ())
            return ()) :: ParIO ())
  S.put b
  rig
  lift (PC.get undefined) -- READ THE IVAR HERE
          
  S.put v
  return undefined
-- this implementation will contain one 'fork' and one 'get'


liftST :: ST s a -> ParVec s elt a
liftST st = undefined
  where
    io = unsafeSTToIO st


-- instance Monad (ParVec s) where
  
instance PC.ParFuture IVar (ParVec s elt) where

instance PC.ParIVar IVar (ParVec s elt) where


--------------------------------------------------------------------------------

p1 :: ST s String
p1 = do
  r <- newSTRef "hi"
  writeSTRef r "hello"
  readSTRef r


p2 :: ParVec s Float String
p2 = do
  r <- liftST$ newSTRef "hi"
  initTheVec 10
  v <- getTheVec

  elem <- liftST$ read v 5

  forkWithVec 5
     (do v1 <- getTheVec
         liftST$ write v1 2 33.3)
     (do v2 <- getTheVec
--          liftST$ read v 25  -- BAD!
         liftST$ write v2 2 44.4)
     
  x <- liftST$ read v 2
  y <- liftST$ read v 7

  liftST$ writeSTRef r "hello"
  liftST$ readSTRef r

