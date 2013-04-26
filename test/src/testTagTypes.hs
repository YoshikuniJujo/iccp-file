import File.Binary
import File.Binary.ICCProfile
import System.Environment
import Data.List
-- import Control.Applicative

import Short

main :: IO ()
main = do
	fin : tagType : _ <- getArgs
	cnt <- readBinaryFile fin
	tgs <- tagTypes cnt
	(_, b) <- readICCP cnt
	print $ public `intersect` tgs
	putStrLn ""
--	print tgs
--	print $ map fst b
	let curvs = map (b !!) $ elemIndices tagType tgs
	putStrLn $ short curvs
	print $ head curvs

public :: [String]
public = [
	"chrm", "clro", "clrt", "curv", "data", "dtim", "mft2", "mft1", "mAB ",
	"mBA ", "meas", "mluc", "mpet", "cvst", "curf", "parf", "samf", "matf",
	"clut", "bACS", "eACS", "ncl2", "para", "pseq", "psid", "rcs2", "sf32",
	"sig ", "text", "uf32", "ui16", "ui32", "ui64", "ui08", "view", "XYZ "
 ]

implemented :: [String]
implemented = [
 ]
