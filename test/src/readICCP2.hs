{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary.ICCProfile

import File.Binary
import System.Environment
import Data.Char
import Numeric

import Short

main :: IO ()
main = do
	fpin : fpout : flags <- getArgs
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
