import File.Binary
import File.Binary.ICCProfile
import System.Environment
import Data.List
-- import Control.Applicative
import Control.Monad

import Short

main :: IO ()
main = do
	fin : tagType : flags <- getArgs
	cnt <- readBinaryFile fin
	tgs <- tagTypes cnt
	(h, b) <- readICCP cnt
	print $ map (\(Tag sig off size) -> (sig, off, size)) $ tags h
	print $ public `intersect` tgs
	putStrLn ""
--	print tgs
--	print $ map fst b
	let curvs = map (b !!) $ elemIndices tagType tgs
	if "1" `elem` flags
		then putStrLn $ short $ head curvs
		else putStrLn $ short curvs
	when ("-s" `notElem` flags) $ print $ head curvs
	when (tagType == "mluc") $ putStrLn $ (\(Unicode16BE t) -> t) $ body_MLUC2 $
		(\(ElemMluc mluc) -> mluc) $ data_body $ snd $ head curvs

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
