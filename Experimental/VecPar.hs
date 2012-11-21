
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}

module VecPar
       -- (
       --   forkParVec, getParVec, initParVec, 
       --   liftST,

       --   p1
       -- )
       where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Par.IO (ParIO,runParIO,IVar)
import qualified Control.Monad.Par.Class as PC
-- import Control.Monad.Trans
-- import qualified Control.Monad.Trans.State.Lazy as SL
import qualified Control.Monad.Trans.State.Strict as S
import Control.Monad.ST
import Data.STRef
import Data.Vector.Mutable as MV 
import System.IO.Unsafe
-- import GHC.IO (unsafeSTToIO)
import Control.Monad.Trans (lift)
import Prelude hiding (read, length)

-- | The ParVec monad.  It uses the StateT monad transformer to layer
-- a state of type (STVector s elt), on top of an inner monad, ParIO.
-- Its third parameter, 'a', is the type inside the ParVec
-- computation, and the 'elt' parameter is the element type of the
-- vector.  The 's' parameter is what's known as a "phantom type".
newtype ParVec s elt a = ParVec ((S.StateT (STVector s elt) ParIO) a)
 deriving Monad

-- | Here, runParVec has a rank-2 type, and the phantom type 's' is
-- bound by the inner forall quantifier.
runParVec :: forall a elt . (forall s . ParVec s elt a) -> a
-- Here we're just using the ParVec value constructor tag to
-- destructure the argument to runParVect.  The result is
-- unsafePerformIO lets us get the needed 'a' out of an IO
-- computation.  The 'st' in (ParVec st) is of the type ((S.StateT
-- (STVector s elt) ParIO) a).
runParVec (ParVec st) = unsafePerformIO io
 where
   -- Create a new mutable vector of length 0 and do a runStateT with
   -- it, getting back a monadic value of type ParIO, which we then
   -- run, getting a value and a final state.  We keep the value and
   -- throw away the state.
   io = do vec <- MV.new 0 -- :: IO (STVector RealWorld elt)
           let x = S.runStateT st vec -- x :: ParIO (a, STVector s0 elt)
           (a,_) <- runParIO x
           return a

-- | getParVec is a ParVec computation that results in the current
-- value of the state, which is of type 'STVector s elt'.
getParVec :: ParVec s elt (STVector s elt)
getParVec = ParVec S.get

-- | initParVec creates a new mutable vector and returns a ParVec
-- computation with that new mutable vector's state as its state.
initParVec :: Int -> ParVec s elt ()
initParVec size = do
  vec <- liftST $ MV.new size
  ParVec $ S.put vec

-- | forkWithVec takes a split point and two ParVec computations.  It
-- gets the state of the current computation, which is a vector, and
-- then divides up that state between the two other computations.
-- Writes to those two computations actually mutate the original
-- vector.
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
--  lift (PC.get undefined) -- READ THE IVAR HERE
          
  -- This 'S.put v' is necessary to make sure that the whole vector
  -- (not just the slices) is available -- v has already been updated
  -- by the updates to the slices.
  S.put v
  return undefined
-- this implementation will contain one 'fork' and one 'get'


liftST :: ST s a -> ParVec s elt a
liftST st = ParVec $ liftIO io
  where
    io = unsafeSTToIO st


-- instance Monad (ParVec s) where
  
instance PC.ParFuture IVar (ParVec s elt) where
-- Implement me...
  
instance PC.ParIVar IVar (ParVec s elt) where
-- Implement me...


--------------------------------------------------------------------------------

p1 :: ST s String
p1 = do
  r <- newSTRef "hi"
  writeSTRef r "hello"
  readSTRef r


p2 :: ParVec s Float String
p2 = do
  r <- liftST$ newSTRef "hi"
  initParVec 10
  v <- getParVec

  elem <- liftST$ read v 5

  forkWithVec 5
     (do v1 <- getParVec
--         liftST$ write v1 9 0 -- BAD! out of bounds
         liftST$ write v1 2 33.3
     )
     (do v2 <- getParVec
--         liftST$ read v 2  -- BAD!
--         liftST$ readSTRef r
         liftST$ write v2 2 44.4)
     
  x <- liftST$ read v 2
  y <- liftST$ read v 7

  liftST$ writeSTRef r "hello "
  hello <- liftST$ readSTRef r

  return$ hello ++ show (x,y)
  

