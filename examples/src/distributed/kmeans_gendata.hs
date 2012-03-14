import System.Random.MWC
import System.Environment
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as U
import Data.Serialize
import Data.ByteString
import KMeansCommon
import Text.Printf

genData nChunks chunkSize = do
    printf "Generating %d chunks of %d points with %d dims\n" nChunks chunkSize vectorSize
    dat <- mapM (\i -> genChunk i chunkSize) [1..nChunks]
--  dat <- generateM nChunks (\i -> genChunk i chunkSize)
    return $ encode dat

loadPoints :: String -> IO (V.Vector (V.Vector Point))
loadPoints filename = do
  bin <- Data.ByteString.readFile filename
  return $ case decode bin of
              Left err -> error $ "Could not deserialize data: "++err
              Right d -> V.fromList d :: V.Vector (V.Vector Point)

testFile filename = do
  pts <- loadPoints filename
  printf "nChunks: %d\n" (V.length pts)
  printf "chunkSize: %d\n" (V.length (pts V.! 0))
  printf "vectorSize: %d\n" (SV.length ((pts V.! 0) V.! 0))
  return ()

main = do
  args <- getArgs
  case args of
    [nChunks, chunkSize, filename] -> genData (read nChunks) (read chunkSize) 
                                        >>= Data.ByteString.writeFile filename
    ["test", filename] -> testFile filename
    otherwise -> print "Usage: kmeans_gendata nChunks chunkSize outputfile\n"
