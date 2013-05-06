import File.Binary.ICCProfile

import System.Environment
import System.FilePath
import File.Binary
import Short
import Control.Applicative
import qualified Data.ByteString.Lazy as BSL

outFilePath :: FilePath -> FilePath
outFilePath infp = takeDirectory infp `combine` "out" `combine` takeFileName infp

main :: IO ()
main = do
	fins <- getArgs
	(readWrite <$> id <*> outFilePath) `mapM_` fins

readWrite :: FilePath -> FilePath -> IO ()
readWrite fin fout = do
--	cnt <- readBinaryFile fin
	cnt <- BSL.readFile fin
--	(ret, dats) <- readICCP cnt
	(ret, rest) <- fromBinary () cnt
	putStrLn "hoge"
	print $ profile_size ret
	putStrLn $ short ret
	putStrLn ""
	print $ sortTags $ tags ret
--	writeBinaryFile fout =<< toBinary () ret
	BSL.writeFile fout =<< toBinary () ret
