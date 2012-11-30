{-# LANGUAGE GeneralizedNewtypeDeriving, PackageImports #-}
{- |
   This module is an alternative version of "Control.Monad.Par" in
   which the `Par` type provides `IO` operations, by means of `liftIO`.
   The price paid is that only `runParIO` is available, not the pure `runPar`.

   This module uses the same default scheduler as "Control.Monad.Par",
   and tasks scheduled by the two can share the same pool of worker
   threads.   
 -}

module Control.Monad.Par.IO
  ( ParIO, P.IVar, runParIO
    -- And instances!               
  )
  where

-- import qualified Control.Monad.Par as P
-- import qualified Control.Monad.Par.Scheds.Trace as P
-- import qualified Control.Monad.Par.Scheds.TraceInternal as TI

import qualified Control.Monad.Par.Scheds.DirectInternal as PI
import qualified Control.Monad.Par.Scheds.Direct as P
import Control.Monad.Par.Class
import Control.Applicative
import "mtl" Control.Monad.Trans (lift, liftIO, MonadIO)

-- | A wrapper around an underlying Par type which allows IO.
newtype ParIO a = ParIO { unPar :: PI.Par a }
  deriving (Functor, Applicative, Monad,
            ParFuture P.IVar, ParIVar P.IVar)

-- | A run method which allows actual IO to occur on top of the Par
--   monad.  Of course this means that all the normal problems of
--   parallel IO computations are present, including nondeterminsm.
--
--   A simple example program:
--
--   >  runParIO (liftIO$ putStrLn "hi" :: ParIO ())
runParIO :: ParIO a -> IO a
runParIO = P.runParIO . unPar

instance MonadIO ParIO where
    liftIO io = ParIO (PI.Par (lift$ lift io))
