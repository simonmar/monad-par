
-- | An associative data structure with a single assignment property similar to IVars (per-key).

module Control.Monad.Par.IMap
 (
  --| Create an empty IMap.
  newImap, 
  --| Publish a value at a specific key, once put the `(key,value)`
  --  binding is permanent and can neither be changed nor removed.
  putKey,
  --| Get the value associated with a key.  `getKey` is a blocking
  --  operation.  If there is not yet an association for the key, it
  --  will wait until one becomes available.
  getKey
 )
 where 

import Control.Monad.Par.Class
import Control.Monad.Par.Unsafe
import Control.Concurrent.QSem
import qualified Data.HashTable.IO as H
-- import qualified Data.HashTable.ST.Basic as H  -- from hashtables package

type HashTable k v = H.BasicHashTable k v

-- We begin with a hashtable and a lock.  We could diffuse contention
-- by a constant factor by splitting this into, say, a vector of hashtables.
-- Further we should probably be using a spinlock.
newtype IMap k iv v = IMap (HashTable k (iv v))

newImap :: (ParUnsafe p iv) => p (IMap k iv v)
newImap = H.new

putKey :: (ParUnsafe p iv) => p ()
putKey = undefined

getKey :: (ParUnsafe p iv) => p ()
getKey = undefined

