{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.ST (ST)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.STRef
import Data.Vector.Mutable as MV
import Data.Vector       (freeze)
import System.IO.Unsafe  (unsafePerformIO)
-- import GHC.IO (unsafeSTToIO)
import Control.Monad.Trans (lift)
import Prelude hiding (read, length)

-- | The ParVec monad.  It uses the StateT monad transformer to layer
-- a state of type (STVector s elt) on top of an inner monad, ParIO.
-- Its third parameter, 'a', is the type inside the ParVec
-- computation, and the 'elt' parameter is the element type of the
-- vector.  The 's' parameter is what's known as a "phantom type".
newtype ParVec s elt a = ParVec ((S.StateT (STVector s elt) ParIO) a)
 deriving Monad

-- | Here, runParVec has a rank-2 type, and the phantom type 's' is
-- bound by the inner forall quantifier.
runParVec :: forall a elt . (forall s . ParVec s elt a) -> a
-- Here we're just using the ParVec value constructor tag to
-- destructure the argument to runParVec.  The 'st' in (ParVec st) is
-- of the type ((S.StateT (STVector s elt) ParIO) a).  The
-- unsafePerformIO lets us get the needed 'a' out of an IO
-- computation.
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
forkWithVec :: forall elt a b s .
               Int
            -> (forall sl . ParVec sl elt a)
            -> (forall sr . ParVec sr elt b)
            -> ParVec s elt (a,b)
forkWithVec mid (ParVec lef) (ParVec rig) = ParVec $ do
  vec <- S.get
  let lvec = slice 0 mid vec
      rvec = slice mid (length v - mid) vec
  lv <- lift$ PC.spawn_$ S.evalStateT lef lvec
  S.put rvec
  rx <- rig                     -- Do the R one on this thread.
  lx <- lift$ PC.get lv         -- Wait for the forked thread to finish.
  S.put vec                     -- Put the whole vec back in place.
  return (lx,rx)


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

  liftST$ set v 0

  forkWithVec 5
     (do v1 <- getParVec
         -- We can't protect against this sort of out-of-bounds error
         -- at compile time -- for that we'd need dependent types.
         -- liftST$ write v1 9 0 -- BAD! out of bounds
         liftST$ write v1 2 33.3
     )
     (do v2 <- getParVec
         -- This, we actually *can* protect against at compile time.
         -- liftST$ read v 2  -- BAD!
         -- liftST$ readSTRef r
         liftST$ write v2 2 44.4)

  z <- liftST$ freeze v

  liftST$ writeSTRef r "hello "
  hello <- liftST$ readSTRef r

--  return$ hello ++ show (x,y)
  return$ hello ++ show z


t1 = unsafeSTToIO p1
t2 = runParVec p2
