{-# LANGUAGE ScopedTypeVariables, RankNTypes, GeneralizedNewtypeDeriving #-}

-- Testing a concept here.

import Control.Monad.State.Strict
import Control.Monad.ST
import Data.STRef
import Unsafe.Coerce (unsafeCoerce)
import Debug.Trace (trace)

-- data MyST s a = MyST State (ST s a)

newtype MyST s a = MyST ((StateT Status (ST s)) a)
--  deriving (MonadState Status)

-- When the computation is closed, no more binds are allowed.  Nobody
-- else is allowed to compute in this context.
data Status = Open | Closed  
  deriving Show

unMyST (MyST a) = a

instance Monad (MyST s) where 
  return x = MyST (return x)
  -- Whenever the status is set to closed, binding further computations becomes an error:
  MyST m >>= (f :: a -> MyST s b) = MyST st
    where 
      st :: (StateT Status (ST s)) b
      st = do
        v1 <- m
        state <- get         
        case trace ("[!!!] Checking status... it's: "++show state) state of 
          Open   -> unMyST (f v1)
          Closed -> error "Attempt to extend closed MyST computation."

runMyST :: forall a . ((forall ss . MyST ss a) -> a)
runMyST (MyST m) = runST (unsafeCoerce st)
 where 
--   st :: ST s a 
--   st :: Int
   st = evalStateT m Open


----------------------------------------------------------------------------------------------------

closeIt :: MyST s ()
closeIt = MyST$ put Closed 

forkIt :: (forall s1 . MyST s1 ()) -> (forall s2 . MyST s2 ()) -> MyST s ()
forkIt (MyST child1) (MyST child2) = MyST $ do
  -- This would fork the children...
  -- Then close off the computation:
  put Closed


-- Example 1, close the computation and then try to do something else.
ex1 :: MyST s String
ex1 = do 
--  newSTRef "hmm"
--  newSTRef "hmm"
  closeIt
  return "hello"


-- Example 2, close at the end.  This is allowed.
ex2 :: MyST s ()
ex2 = do 
  return "hello"  
  return "hello"  
  return "hello"  
  closeIt
  
