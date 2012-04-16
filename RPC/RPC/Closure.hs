{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

-- | A simple type to represent a closure, that is, a function
-- and its environment. The current implementation represents
-- functions as strings, but this could be theoretically
-- swapped out for the \"static\" mechanism described in the
-- paper.
module RPC.Closure (
                       Closure(..),
		       BiClosure,
                       ) where

import Data.Typeable (Typeable)
import RPC.Encoding (Payload)

-- import qualified Data.Binary as Bin -- (Binary,get,put)
import qualified Data.Serialize as Ser

-- | A data type representing a closure, that is, a function with its environment.
--   In spirit, this is actually:
--    
-- >   data Closure a where
-- >     Closure :: Serializable v => Static (v -> a) -> v -> Closure a     
--
--   where the Static type wraps a function with no non-static free variables.
--   We simulate this behavior by identifying top-level functions as strings.
--   See the paper for clarification.
data Closure a = Closure String Payload
     deriving (Typeable)

-- | This type represents a serialized AND native (local) version of a
--   value.  The consumer can use whichever is appropriate.
type BiClosure a = (a, Closure a)


instance Show (Closure a) where
     show a = case a of
                (Closure fn _pl) -> show fn

{-
instance Bin.Binary (Closure a) where
     get = do s <- Bin.get
              v <- Bin.get
              return $ Closure s v 
     put (Closure s v) = Bin.put s >> Bin.put v
-}


instance Ser.Serialize (Closure a) where
     get = do s <- Ser.get
              v <- Ser.get
              return $ Closure s v 
     put (Closure s v) = Ser.put s >> Ser.put v

