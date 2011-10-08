{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances, TypeSynonymInstances, ScopedTypeVariables,
	     EmptyDataDecls
  #-}
{-| 

UNFINISHED 

    This module provides a basic implementation of channels that is
    usable with any monad satisfying ParIVar.
 -}

module Control.Monad.Par.Chan where 

import Control.Monad.Par.Class as PC
import Control.Monad.Trans
import Control.Monad.Trans.State as S
import Control.Monad.ST
import Control.Monad.Par.OpenList as L
-- import qualified Data.Vector.Unboxed as U
import Data.Vector as V
import Data.Int
import Data.BitList

--------------------------------------------------------------------------------
-- Make Par computations with state work.
-- (TODO: move these instances to a different module.)

class SplittableState a where
  splitState :: a -> (a,a)

instance ParGettable p iv => ParGettable (StateT s p) iv where
  get = lift . PC.get

instance (SplittableState s, ParIVar p iv) 
      =>  ParIVar (StateT s p) iv 
 where
  fork (task :: StateT s p ()) = 
              do s <- S.get 
                 let (s1,s2) = splitState s
                 S.put s2
                 lift$ PC.fork $ do
		   runStateT task s1
                   return ()
                 return ()
  new      = lift PC.new
  put_ v x = lift$ PC.put_ v x
  newFull_ = lift . PC.newFull_


--instance ParGettable p iv => ParGettable (ST s p) iv where
--  get = lift . PC.get

--------------------------------------------------------------------------------

-- Unique keys for strings:
-- Two options:
--   * unsafely generate unique values 
--      (unsafePerformIO or a non-standard ST monad tranfsormer)
--   * use a counter + the tree-index in the fork-tree

-- unsafeGensym


-- Let's do the safe one first:

type Key = (Int, BitList)
-- genKey :: Monad m => ParC m Key 
genKey = undefined

--------------------------------------------------------------------------------

-- A Par monad with stream support can be built from any other Par monad:
type ParC p = StateT (CursorMap Magic) p 

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

-- instance ParIVar m v => ParChan m (SendPort v) (RecvPort v) where 
-- instance ParIVar m v => ParChan m SendPort RecvPort where 
instance ParIVar m v => ParChan (ParC m) SendPort RecvPtr where 

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
