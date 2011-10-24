{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- If we instead commute the monad transformers we can change the
-- reader value observed by a continuation.

import Control.Monad.Cont as C
import qualified Control.Monad.Reader as R
import Data.IORef
import Text.Printf

type M a = C.ContT () (R.ReaderT Int IO) a

test ref = do 
  x <- R.ask 
  liftIO$ printf "Observed value %d before callCC\n" x 
  callCC$ \cont -> do
     liftIO$ writeIORef ref cont
     liftIO$ printf "Write ioref inside callcc...\n" 
  z <- R.ask 
  liftIO$ printf "Observed value %d in invoked continuation\n" z
  return ()

main = do ref :: IORef (() -> M ()) <- newIORef (error "should not be used")
          let m0 = test ref
	      m1 = C.runContT m0 (\ () -> return ())
	      m2 = R.runReaderT m1 (100::Int)
          m2 

          k <- readIORef ref
          let m3 = do 
		      w <- lift$ R.ask 
		      liftIO$ putStrLn ("In new runReader instance: observed " ++ show w)
		      k ()
		      liftIO$ putStrLn  " !! Should not reach here..."

	  R.runReaderT (C.runContT m3 (\ () -> return ())) 200
          putStrLn "Done with main."
