{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
   This module is an alternative version of "Control.Monad.Par" in
   which the `Par` type provides `IO` operations, by means of `liftIO`.
   The price paid is that only `runParIO` is available, not the pure `runPar`.

   This module uses the same default scheduler as "Control.Monad.Par".
 -}

module Control.Monad.Par.IO
  ( ParIO, IVar, runParIO
    -- And instances!               
  )
  where

import Control.Monad.Par.Scheds.Trace (Par, IVar)
import qualified Control.Monad.Par.Scheds.TraceInternal as Internal

import Control.Monad.Par.Class
import Control.Applicative
import Control.Monad.Trans (liftIO, MonadIO)

-- | A wrapper around an underlying Par type which allows IO.
newtype ParIO a = ParIO (Par a)
  deriving (Functor, Applicative, Monad, ParFuture IVar, ParIVar IVar)

-- | A run method which allows actual IO to occur on top of the Par
--   monad.  Of course this means that all the normal problems of
--   parallel IO computations are present, including nondeterminsm.
--
--   A simple example program:
--
--   >  runParIO (liftIO $ putStrLn "hi" :: ParIO ())
runParIO :: ParIO a -> IO a
runParIO (ParIO p) = Internal.runParIO p

instance MonadIO ParIO where
    liftIO io = ParIO (Internal.Par (Internal.LiftIO io))
