
-- ILists: lists whose cdr fields are IVars, filled in asynchronously
-- via Par computation.

-- This module mostly exists as a datatype to use in building other,
-- higher-level abstractions.  (See OpenList and Stream.)

module Control.Monad.Par.IList 
 (
   IList(..)
 )
where

import Control.Monad.Par
import Control.DeepSeq

data IList a = Null | Cons { hd :: a, tl :: IVar (IList a) }

-- | To fully evaluate an open list means to evaluate all the
--   car field.  There is nothing to be done about the fact
--   that the trailing IVar cdr field may receive further extensions.
instance NFData a => NFData (IList a) where 
--  rnf Null = r0
  rnf Null = ()
  rnf (Cons a b) = rnf a `seq` rnf b
