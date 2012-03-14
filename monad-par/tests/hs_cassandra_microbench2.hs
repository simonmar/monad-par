
-- Peculiar:
--    * Requires hscassandra 0.0.8, available on github only

-- Functions to:
--    * Connect to a cassandra nosql server
--    * Put a bytestring
--    * Get a bytestring

-- This file acts as a microbench for insertion or retrieval of keys

import Database.Cassandra
import Database.Cassandra.Types
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Database.Cassandra.Thrift.Cassandra_Types as CT
import Data.Either.Utils
import qualified Data.List as L
import System.Time
import Data.Time.Clock
import System.Environment
import Control.Monad
import qualified Data.Set as S


config = CassandraConfig { cassandraKeyspace = "Test2"
                     , cassandraConsistencyLevel = ONE
                     , cassandraHostname = "127.0.0.1"
                     , cassandraPort = 9160
                     , cassandraUsername = ""
                     , cassandraPassword = ""
                     }

pack :: [Char] -> BS.ByteString
pack string = BS.pack string
     
deColumnValue :: Column -> BS.ByteString
deColumnValue (Column _ value _) = value
                 
fetchValue string = runCassandraT config $ do
  res <- get "Users" (pack string) AllColumns
  return $ deColumnValue $ head $ res

insertValue string byteString = runCassandraT config $ do
  insert "Users" (pack string) [ (pack "name") =: byteString ]
  
removeValue string = runCassandraT config $ do
  remove "Users" (pack string) (ColNames [ (pack "name") ])

main = do
    
    let words = map (\n -> (show n) ++ "Thequickbrownfoxjumpedoverthelazydog")  $ take 50000 [1..]
        
    putStrLn "Benchmarking Cassandra binding."
  
    putStrLn "About to start timing writes..."
  
    writeStart <- getCurrentTime
    
    runCassandraT config $ do
      mapM_ (\w -> insert "Users" (pack w) [(pack "name") =: (pack w) ]) words
    
    writeStop <- getCurrentTime
  
    let writeTime = diffUTCTime writeStop writeStart
  
    putStrLn ("Writes took " ++ (show $ writeTime))
  
    putStrLn "Starting reads..."
  
    readStart <- getCurrentTime
    
    runCassandraT config $ do
      mapM_ (\w -> get "Users" (pack w) AllColumns) words

    readStop <- getCurrentTime
  
    let readTime = diffUTCTime readStop readStart
  
    putStrLn ("Reads took " ++ (show $ readTime))
  
    let totalTime = diffUTCTime readStop writeStart
  
    putStrLn ("Total time: " ++ (show $ totalTime))
  
    putStrLn "Removing keys..."
  
    runCassandraT config $ do
      mapM_ (\w -> remove "Users" (pack w) (ColNames [ (pack "name") ])) words
  
  
  