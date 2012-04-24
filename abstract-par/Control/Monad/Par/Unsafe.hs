{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
-- TODO: ADD Unsafe

-- | Unsafe operations.  NOT part of "Safe Haskell".
-- 
-- These are "unsafe" (in the normal, Haskell sense) when used with a
-- "runPar" of type `Par a -> a`.  If used with a `runParIO` that
-- stays in the IO monad, then they are simply dangerous.
-- 
-- For the purposes of Safe Haskell, any module that imports this
-- module becomes untrustworthy.

module Control.Monad.Par.Unsafe 
  (
   ParUnsafe(..)
  ) 
where

-- import Control.Monad.Par.Class

-- | The class of Par monads that provide unsafe functionality.
class ParUnsafe iv p | p -> iv where 
  -- | Peek at the current contents of an 'IVar' in a nonblocking way.
  unsafePeek   :: iv a -> p (Maybe a)

  -- | Attempt to put a value into an 'IVar'.  If successful, return the
  --   value put.  If something is already there, return it instead.
  unsafeTryPut :: iv a -> a -> p a

  -- | Lift an 'IO' operation into the Par monad.
  unsafeParIO  :: IO a -> p a

-- Aside:
-- If the need ever arises we could also consider unsafeMultiplePut that
-- would be able to change the current value of an IVar.  It could
-- cause big problems in the distributed case, however.
