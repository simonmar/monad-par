
-- | INTERNAL module.
-- 
-- ILists are lists whose cdr fields are IVars, filled in
-- asynchronously via Par computation.  This module mostly exists as a
-- datatype to use in building other, higher-level abstractions.  (See
-- OpenList and Stream.)

module Control.Monad.Par.IList 
 (
   IList(..)
 )
where

import Control.Monad.Par
import Control.DeepSeq

-- | An 'IList' is the equivalent of a lazy list in the 'Par' monad.
-- The tail of the list is an 'IVar', which allows the list to be
-- produced and consumed in parallel.
data IList a = Null | Cons { hd :: a, tl :: IVar (IList a) }

-- | To fully evaluate an 'IList' means to evaluate both the head
-- and tail.  This does not evaluate the entire spine of the list
-- of course, because the tail is an 'IVar'.
instance NFData a => NFData (IList a) where
--  rnf Null = r0
  rnf Null = ()
  rnf (Cons a b) = rnf a `seq` rnf b
