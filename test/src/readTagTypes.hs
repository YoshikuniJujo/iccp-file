{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary.ICCProfile

import File.Binary
import System.Environment
import Control.Applicative
import Data.List

main :: IO ()
main = do
	fpins <- getArgs
	cnt <- mapM (readBinaryFile) fpins
	used <- nub . concat <$> mapM tagTypes cnt

	putStrLn $ "not publics : " ++ unwords (used \\ public)
	putStrLn $ "public, used: " ++ unwords (public `intersect` used)
	putStrLn ""
	putStrLn $ "not used    : " ++ intercalate "\n              "
		(map unwords $ groupN 10 $ public \\ used)
	putStrLn ""
	putStrLn $ "public, used, not-implemented:\n              " ++
		unwords (public `intersect` used \\ implemented)
	putStrLn $ show (100 * length implemented `div` length public) ++ "%"

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)

public :: [String]
public = [
	"chrm", "clro", "clrt", "curv", "data", "dtim", "mft2", "mft1", "mAB ",
	"mBA ", "meas", "mluc", "mpet", "cvst", "curf", "parf", "samf", "matf",
	"clut", "bACS", "eACS", "ncl2", "para", "pseq", "psid", "rcs2", "sf32",
	"sig ", "text", "uf32", "ui16", "ui32", "ui64", "ui08", "view", "XYZ "
 ]

implemented :: [String]
implemented = [
	"curv", "data", "mft2", "mAB "
 ]
