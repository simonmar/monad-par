
{-| This is a convenience module that reexports a default scheduler
    and Par type classes and instances.
 -}

module Control.Monad.Par 
 (
   module Control.Monad.Par.Class,
   module Control.Monad.Par.Scheds.Trace 
 )
where 

import Control.Monad.Par.Class
import Control.Monad.Par.Scheds.Trace hiding (spawn_, spawn, put, get, new, newFull, fork, put_, newFull_)


