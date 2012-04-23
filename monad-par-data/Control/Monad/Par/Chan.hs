{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, 
             UndecidableInstances, TypeSynonymInstances, ScopedTypeVariables,
	     EmptyDataDecls, CPP
  #-}
{-| 

UNFINISHED 

    This module provides a basic implementation of channels that is
    usable with any monad satisfying ParIVar.
 -}


module Control.Monad.Par.Chan 
 (
   ParC, 
--    runParRNG, fork,
--    IVar, new, newFull, newFull_, get, put, put_,
--    spawn, spawn_
 )
 where 

import qualified  Control.Monad.Par.Class as PC
import Control.Monad.Trans
import qualified Control.Monad.Trans.State as S
import Control.Monad.ST
import Control.Monad.Par.OpenList as L
import Control.DeepSeq
import Control.Parallel (pseq)
-- import qualified Data.Vector.Unboxed as U
import Data.Vector as V
import Data.Int
import Data.BitList

--------------------------------------------------------------------------------
-- Make Par computations with state work.
-- (TODO: move these instances to a different module.)

-- | The @SplittableState@ class models state that can be split along
--   with a Par monad's control flow.  It would be possible to simply
--   duplicate the state irrespective of its type, but this interface
--   allows the definition of custom (non-duplication) splitting
--   behaviors, such as splitting a random number generator.
class SplittableState a where
  splitState :: a -> (a,a)

-- include "Scheds/par_instance_boilerplate.hs"

-- Allow State to be added to any ParFuture monad:
-- This could be used for RNG.
instance (SplittableState s, PC.ParFuture iv p)
      =>  PC.ParFuture iv (S.StateT s p) where
  get   = lift . PC.get
  spawn_ (task :: S.StateT s p a) = 
		  do s <- S.get 
		     let (s1,s2) = splitState s
		     S.put s2
		     let x  :: p (a,s) = S.runStateT task s1
                         x' :: p a     = do (x,_) <- x; return x
		     lift$ PC.spawn_ x'
  spawn p = PC.spawn_ (do p' <- p; pseq (rnf p') return p' )

  spawnP = error "unimplemented"

--  spawn = spawn
--  spawn p  = do r <- new;  fork (p >>= put r);   return r
--  spawn_ p = do r <- new;  fork (p >>= put_ r);  return r

instance (SplittableState s, PC.ParIVar iv p) 
      =>  PC.ParIVar iv (S.StateT s p) 
 where
  fork     = fork
  new      = new
  put_     = put_
  newFull_ = lift . PC.newFull_
  newFull  = lift . PC.newFull

new :: PC.ParIVar iv p => S.StateT s p (iv a)
new      = lift  PC.new

put  v x = lift$ PC.put  v x
put_ v x = lift$ PC.put_ v x

fork :: (SplittableState s, PC.ParIVar iv p) 
     => S.StateT s p () -> S.StateT s p ()
fork task = 
		do s <- S.get 
		   let (s1,s2) = splitState s
		   S.put s2
		   lift$ PC.fork $ do
		     S.runStateT task s1
		     return ()
		   return ()



--------------------------------------------------------------------------------

-- Unique keys for strings:
-- Two options:
--   * unsafely generate unique values, e.g. Data.Unique
--      (unsafePerformIO or a non-standard ST monad tranfsormer)
--   * use a counter + the tree-index in the fork-tree

-- unsafeGensym


-- Let's do the safe one first:

type Key = (Int, BitList)
-- genKey :: Monad m => ParC m Key 
genKey = undefined

--------------------------------------------------------------------------------

-- A Par monad with stream support can be built from any other Par monad:
type ParC p = S.StateT (CursorMap Magic) p 

-- newtype CursorMap a = CursorMap (Vector (Cursor a))
--data Cursor    a = Cursor Int (OpenList a)
--newtype RecvPtr a = RecvPtr Int
newtype CursorMap a = CursorMap (Vector (OpenList Magic))
--newtype RecvPtr a = RecvPtr (Int, OpenList a)
newtype RecvPtr a = RecvPtr (Int, a)

data Magic

instance SplittableState (CursorMap a) where 
  splitState x = (x,x)

-- type SendPort v a = ()
-- type RecvPort v a = ()
data SendPort a = SendPort a
data RecvPort a = RecvPort a

instance PC.ParIVar v m => PC.ParChan (ParC m) SendPort RecvPtr where 

  -- A new channel coins a unique key that can be used to lookup the stream.
  newChan = undefined
  send    = undefined

  -- Advance the cursor:
  recv (RecvPtr (i,_::elt)) = 
    -- The RecvPtr essentially carries the type, but no value for the stream.
    do CursorMap vec <- S.get 
       let ol = unsafeIndex vec i       
           hd = L.head ol
--       tl <- lift$ L.tail ol
--       V.modify (\v -> write v i tl) vec 

--       return hd
       return undefined
