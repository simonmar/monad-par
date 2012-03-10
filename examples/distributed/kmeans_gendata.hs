import System.Random.MWC
import System.Environment
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as U
import Data.Serialize
import Data.ByteString
import KMeansCommon

genData nChunks chunkSize = do
    dat <- mapM (\i -> genChunk i chunkSize) [1..nChunks]
--  dat <- generateM nChunks (\i -> genChunk i chunkSize)
    return $ encode dat

main = do
  args <- getArgs
  case args of
    [nChunks, chunkSize, filename] -> genData (read nChunks) (read chunkSize) 
                                        >>= Data.ByteString.writeFile filename
    otherwise -> print "Usage: kmeans_gendata nChunks chunkSize outputfile\n"
