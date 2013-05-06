{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary.ICCProfile

import File.Binary
import System.Environment
import Data.Char
import Numeric

import Short
import System.FilePath

import Control.Monad

main :: IO ()
main = do
	args <- getArgs
	let	(flags, fpins) = case args of
			"--md5sum-0" : is -> (["--md5sum-0"], is)
			is -> ([], is)
		fpouts = map outFilePath fpins
	zipWithM_ (reading flags) fpins fpouts
	print (fpins, fpouts)

outFilePath :: FilePath -> FilePath
outFilePath infp =
	takeDirectory infp `combine` "out" `combine` takeFileName infp

-- main :: IO ()
-- main = do
--	fpin : fpout : flags <- getArgs
reading :: [String] -> FilePath -> FilePath -> IO ()
reading flags fpin fpout = do
{-
	args <- getArgs
	let	(flags, fpin) = case args of
			"--md5sum-0" : i : _ -> (["--md5sum-0"], i)
			i : _ -> ([], i)
		fpout = takeDirectory fpin `combine` "out" `combine`
			takeFileName fpin
-}
	cnt <- readBinaryFile fpin
	(ret, dats) <- readICCP cnt
	print ret
	putStrLn $ short dats
	putStrLn ""
	print $ map tag_signature $ tags ret
	putStrLn ""
-- *	putStrLn $ short dats
	putStrLn $ strShowHex $ profile_identifier ret

	print "hage"
	let Just bin = writeICCP (ret, dats) :: Maybe String
--	print (bin :: String)
--	print =<< writeICCP (ret, dats)
{-
	writeBinaryFile (fpout ++ "add_1") . (++ "\0") =<<
		writeICCP (ret{ profile_identifier = replicate 16 '\0' }, dats)
-}
	let ret' = if "--md5-0" `elem` flags
		then ret { profile_identifier = replicate 16 '\0' }
		else ret
-- *	print dats
	writeBinaryFile fpout =<< writeICCP (ret', dats)
--		writeICCP (ret{ profile_identifier = replicate 16 '\0' }, dats)
--	putStrLn $ take 1000 $ drop 6000 $ show cnt
--	putStrLn $ take 1000 $ drop 6000 $ show bin
--	putStrLn $ take 1000 $ drop 29100 $ show cnt
--	putStrLn $ take 1000 $ drop 29100 $ show bin
--	putStrLn $ take 1000 $ drop 30000 $ show cnt
--	putStrLn $ take 1000 $ drop 30000 $ show bin
	print fpin
	print $ paddings $ tags ret
	print $ duplicate [] $ tags ret
	print $ map tag_element_size $ tags ret
--	print $ map length =<< fromElems dats
{-
	print $ sizes $ tags ret
	print $ sum $ sizes $ tags ret
	print $ profile_size ret - sum (sizes $ tags ret)
	print $ tag_count ret * 12 + 4
-}
	print $ filePadding ret
	print $ profile_size ret
	print $ last $ tags ret
	print $ map tag_signature $ tags ret
{-
	putStrLn $ take 1000 $ drop 1550000 $ show cnt
	putStrLn $ take 1000 $ drop 1550000 $ show bin
-}

strShowHex = concatMap $ toTwo . flip showHex "" . ord

toTwo s = replicate (2 - length s) '0' ++ s
