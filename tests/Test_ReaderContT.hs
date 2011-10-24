{-# LANGUAGE CPP #-}


import Control.Monad.Cont as C
import qualified Control.Monad.Reader as R
import Data.IORef
import Text.Printf

type M a = R.ReaderT Int ContIO a
type ContIO = C.ContT () IO

test ref = do 
  x <- R.ask 
  liftIO$ printf "Observed value %d before callCC\n" x 
  callCC$ \cont -> do
     y <- R.ask 
     liftIO$ writeIORef ref cont
     liftIO$ printf "Observed value %d inside callCC\n" y
  z <- R.ask 
  liftIO$ printf "Observed value %d in invoked continuation\n" z
  return ()

main = do ref <- newIORef (error "unused")
          let test' = do test ref
	      m1 = do R.runReaderT test' (100::Int)

          C.runContT m1 (\ () -> return ())
          k <- readIORef ref
          let m2 = do w <- R.ask 
		      liftIO$ putStrLn ("  In new runReader instance: observed " ++ show w)
		      k ()
		      liftIO$ putStrLn " !! Should not reach here..."

          C.runContT (R.runReaderT m2 200) (\ () -> return ())
          putStrLn "Done with main."
