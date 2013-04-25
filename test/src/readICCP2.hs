{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary.ICCProfile

import File.Binary
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- readBinaryFile fp
	(ret, dats) <- readICCP cnt
	print ret
	putStrLn ""
	print $ map tag_signature $ tags ret
	putStrLn ""
	putStrLn $ short dats
