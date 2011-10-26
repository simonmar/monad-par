

-- Helper routines used by other scripts in this directory.

module ScriptHelpers
 ( trim
 , remComments
 , unlessM 
 , indent
 , runEcho
 , inDirectory
 , readDataFile
 ) 
where

import HSH
import Data.Char
import Control.Monad
import System.Directory

-- | Remove comments from a list of lines.
-- Assumes hash is the comment character.
remComments :: [String] -> [String]
remComments ls = filter (pred . stripLeadingWS) ls
 where 
  commentChars = "#"
  pred str = not (take (length commentChars) str == commentChars) 
  stripLeadingWS []                = [] 
  stripLeadingWS (w:t) | isSpace w = stripLeadingWS t
  stripLeadingWS ls                = ls

-- Read our simple whitespace-separated data files:
readDataFile :: String -> IO [[String]]
readDataFile file = 
  do str <- readFile file
     return$ filter (not . null) $ 
             map words $
	     remComments $ lines str  

-- remComments :: String -> [String] -> [String]
-- remComments commentchars ls = filter (pred . stripLeadingWhitespace) ls
--  where 
--   pred str = not (take (length commentchars) str == commentchars) 
--   stripLeadingWhitespace []      = [] 
--   stripLeadingWhitespace (' ':t) = stripLeadingWhitespace t
--   stripLeadingWhitespace ls      = ls

-- | Trim whitespace from both ends of a string.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace


inDirectory dir action = do
  d1 <- getCurrentDirectory
  setCurrentDirectory dir
  x <- action
  setCurrentDirectory d1
  return x

runEcho cmd = do putStrLn$ "  Running: "++ cmd
		 runIO $ cmd -|- indent

unlessM m1 m2 = do x <- m1; unless x m2

indent = ("   "++)
