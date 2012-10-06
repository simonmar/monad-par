{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |

   This module is an alternative version of "Control.Monad.Par" in
   which the `Par` type provides `IO` operations, by means of `liftIO`.
   The price paid is that only `runParIO` is available, not the pure `runPar`.

   This module uses the same default scheduler as "Control.Monad.Par",
   and tasks scheduled by the two can share the same pool of worker
   threads.

-}

module Control.Monad.Par.IO
  ( ParIO, P.IVar,
    runParIO )
  where

-- import qualified Control.Monad.Par as P
-- import qualified Control.Monad.Par.Scheds.Trace as P
-- import qualified Control.Monad.Par.Scheds.TraceInternal as TI

import qualified Control.Monad.Par.Scheds.Direct as P
import Control.Monad.Par.Class
import Control.Applicative
import Control.Monad.IO.Class

newtype ParIO a = ParIO { unPar :: P.Par a }
  deriving (Functor, Applicative, Monad,
            ParFuture P.IVar, ParIVar P.IVar)

runParIO = P.runParIO . unPar

instance MonadIO ParIO where
    liftIO io = ParIO (P.Par (lift$ lift io))
--  liftIO (ParIO (P.Par contRdr)) = undefined

-- newtype Par a = Par { unPar :: C.ContT () ROnly a }
--     deriving (Monad, MonadCont, RD.MonadReader Sched)
-- type ROnly = RD.ReaderT Sched IO

-- newtype Par a = Par {
--     runCont :: (a -> Trace) -> Trace
-- }

  
--   import qualified Control.Monad.Par.Class as PC
-- import Control.Monad.Par.Scheds.TraceInternal
-- import Control.DeepSeq
-- import Control.Monad as M hiding (mapM, sequence, join)
-- import Prelude hiding (mapM, sequence, head,tail)

-- -- -----------------------------------------------------------------------------

-- -- Not in 6.12: {- INLINABLE fork -}
-- {-# INLINE fork #-}
-- fork :: Par () -> Par ()
-- fork p = Par $ \c -> Fork (runCont p (\_ -> Done)) (c ())

-- -- --------------------------------------------------------------------------------
-- -- -- Standard instances:

-- -- <boilerplate>
-- spawn p  = do r <- new;  fork (p >>= put r);   return r
-- spawn_ p = do r <- new;  fork (p >>= put_ r);  return r
-- -- </boilerplate>>

-- spawnP :: NFData a => a -> Par (IVar a)
-- spawnP a = spawn (return a)

-- instance PC.ParFuture IVar Par  where
--   get    = get
--   spawn  = spawn
--   spawn_ = spawn_
--   spawnP = spawnP

-- instance PC.ParIVar IVar Par  where 
--   fork = fork 
--   new  = new
--   put  = put
--   put_ = put_
--   newFull  = newFull
--   newFull_ = newFull_
-- --  yield = yield
