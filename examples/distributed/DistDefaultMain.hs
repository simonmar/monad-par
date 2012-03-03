

module DistDefaultMain (defaultMain) where


import Control.Monad.Par.Meta.Dist (longSpawn, Par, get, shutdownDist, WhichTransport(Pipes,TCP),
				    runParDistWithTransport, runParSlaveWithTransport)
import System.Environment

-- defaultMain metadat parcomp defaults = do 
--defaultMain metadat parcomp defaults parseargs = do 
defaultMain metadat parcomp numargs parseargs = do 
    args <- getArgs
    let (version, trans_, rest) = case args of 
            []        -> ("master", "pipes", [])
            [v]       -> (v,        "pipes", [])
            [v,t]     -> (v,         t,      [])
            v:t:rest  -> (v,         t,      rest)
        trans = parse trans_
        parse "tcp"   = TCP
	parse "pipes" = Pipes

--    args <- mapM (\(f,x) -> f x) (zip defaults rest)
    let 
    -- TODO: How can we get the ranks here?  Need to parse MACHINE_LIST
        ranks = 4 

--    let strs = rest ++ drop (length rest) defaults
    let strs = take numargs (rest ++ repeat "")
    args <- parseargs ranks strs

    case version of 
        "slave" -> runParSlaveWithTransport metadat trans
        "master" -> do 
		       ans <- runParDistWithTransport metadat trans (parcomp args)
		       putStrLn $ "Final answer: " ++ show ans
		       putStrLn $ "Calling SHUTDOWN..."
                       shutdownDist
		       putStrLn $ "... returned from shutdown, apparently successful."
        str -> error$"Unhandled mode: " ++ str
