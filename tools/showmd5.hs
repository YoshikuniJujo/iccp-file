import System.Cmd
import System.Environment
import Control.Monad
import System.FilePath

main :: IO ()
main = do
	fps <- getArgs
	forM fps $ \fp -> do
		rawSystem "md5sum" [fp]
		rawSystem "md5sum" [outFilePath fp]
		putStrLn ""
	return ()

outFilePath infp =
	takeDirectory infp `combine` "out" `combine` takeFileName infp
