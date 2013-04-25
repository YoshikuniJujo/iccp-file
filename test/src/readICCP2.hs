{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary.ICCProfile

import File.Binary
import System.Environment
import Data.Char
import Numeric

main :: IO ()
main = do
	fpin : fpout : _ <- getArgs
	cnt <- readBinaryFile fpin
	(ret, dats) <- readICCP cnt
	print ret
	putStrLn ""
	print $ map tag_signature $ tags ret
	putStrLn ""
	putStrLn $ short dats
	putStrLn $ strShowHex $ profile_identifier ret

	print "hage"
	let Just bin = writeICCP (ret, dats) :: Maybe String
--	print (bin :: String)
--	print =<< writeICCP (ret, dats)
	writeBinaryFile (fpout ++ "add_1") . (++ "\0") =<<
		writeICCP (ret{ profile_identifier = replicate 16 '\0' }, dats)
	writeBinaryFile fpout =<<
		writeICCP (ret{ profile_identifier = replicate 16 '\0' }, dats)
	putStrLn $ take 1000 $ drop 6000 $ show cnt
	putStrLn $ take 1000 $ drop 6000 $ show bin
--	putStrLn $ take 1000 $ drop 29100 $ show cnt
--	putStrLn $ take 1000 $ drop 29100 $ show bin
--	putStrLn $ take 1000 $ drop 30000 $ show cnt
--	putStrLn $ take 1000 $ drop 30000 $ show bin
	print $ paddings $ tags ret
	print $ duplicate [] $ tags ret
	print $ map tag_element_size $ tags ret
	print $ map length =<< fromElems dats
{-
	print $ sizes $ tags ret
	print $ sum $ sizes $ tags ret
	print $ profile_size ret - sum (sizes $ tags ret)
	print $ tag_count ret * 12 + 4
-}

strShowHex = concatMap $ toTwo . flip showHex "" . ord

toTwo s = replicate (2 - length s) '0' ++ s
