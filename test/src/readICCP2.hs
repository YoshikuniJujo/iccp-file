{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary.ICCProfile (ICCP, getElement, tags, tag_signature, short)

import File.Binary
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- readBinaryFile fp
	let Right (ret, _) = fromBinary () cnt :: Either String (ICCP, String)
	print ret
	putStrLn ""
	print $ map tag_signature $ tags ret
	putStrLn ""
	putStrLn $ short $ map (($ cnt) . getElement) $ take 17 $ tags ret
