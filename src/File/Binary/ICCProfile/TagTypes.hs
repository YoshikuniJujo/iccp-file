{-# LANGUAGE QuasiQuotes, TypeFamilies, EmptyDataDecls, PatternGuards #-}

module File.Binary.ICCProfile.TagTypes (
	Body(..),
	Elem(..),
	Curv(..),
	Data(..),
	MFT2(..),
	MBA(..),
	MAB(..),
	MAB_(..),
	MAB_CLUT(..),
	MLUC_Pre(..),
	MLUC(..),
	MLUC_RECORD(..),
	module File.Binary.ICCProfile.TagTypes_yet
) where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()

import File.Binary.ICCProfile.TagTypes_yet

import Control.Applicative
import Control.Arrow

import Data.Word
import Data.Fixed

import qualified Data.Text as Text
import Data.Text.Encoding

import qualified Data.ByteString as BS

data S15Fixed16Number = S15Fixed16Number (Fixed B16)

instance Show S15Fixed16Number where
	show (S15Fixed16Number f) = show f

data B16

instance HasResolution B16 where
	resolution _ = 2 ^ (16 :: Int)

instance Field S15Fixed16Number where
	type FieldArgument S15Fixed16Number = ()
	fromBinary () b = first (S15Fixed16Number . (/ 2 ^ (16 :: Int)) .
		(fromIntegral :: Int -> Fixed B16)) <$> fromBinary 4 b
	toBinary () (S15Fixed16Number f) = toBinary 4
		(floor $ f * 2 ^ (16 :: Int) :: Int)

type UInt16Number = Int
type UInt32Number_ = Int
type UInt8Number = Int
type UInt8Or16Number = Int

getPadding :: Int -> Int
getPadding n
	| m <- n `mod` 4, m /= 0 = 4 - m
	| otherwise = 0

[binary|

Body

arg :: ([String], Int)

replicate 4 (){String}: data_type
4: 0
(data_type, snd arg - 8){Elem}: data_body
getPadding $ getSize (snd arg) data_body: 0 -- padd

|]

getSize :: Int -> Elem -> Int
getSize _ (ElemCurv e) = 4 + 2 * num_curv e
getSize arg _ = arg

instance Show Body where
	show dat = "Body (" ++ show (data_body dat) ++ ")"
{-
	show dat = "(Data " ++
		show (data_type dat) ++ " " ++
		"(" ++ show (data_body dat) ++ "))"
-}

data Elem
	= ElemCurv Curv
	| ElemData Data
	| ElemMFT2 MFT2
	| ElemMAB  MAB
	| ElemMBA  MBA
	| ElemText Text2
	| ElemXYZ XYZ2
	| ElemDesc Desc
	| ElemChad CHAD2
	| ElemMluc MLUC
	| ElemMmod MMOD2
	| ElemPara Para2
	| ElemVCGT VCGT2
	| ElemNDIN NDIN2
	| ElemOthers String String
	deriving Show

instance Field Elem where
	type FieldArgument Elem = (String, Int)
	fromBinary ("curv", size) =
		fmap (first ElemCurv) . fromBinary size
	fromBinary ("data", size) =
		fmap (first ElemData) . fromBinary size
	fromBinary ("mft2", size) =
		fmap (first ElemMFT2) . fromBinary size
	fromBinary ("mAB ", size) =
		fmap (first ElemMAB ) . fromBinary size
	fromBinary ("mBA ", size) =
		fmap (first ElemMBA ) . fromBinary size
	fromBinary ("XYZ ", size) =
		fmap (first ElemXYZ) . fromBinary size
	fromBinary ("sf32", size) =
		fmap (first ElemChad) . fromBinary size
	fromBinary ("text", size) =
		fmap (first ElemText) . fromBinary size
	fromBinary ("desc", size) =
		fmap (first ElemDesc) . fromBinary size
	fromBinary ("mluc", size) =
		fmap (first ElemMluc) . fromBinary size
	fromBinary ("mmod", size) =
		fmap (first ElemMmod) . fromBinary size
	fromBinary ("para", size) =
		fmap (first ElemPara) . fromBinary size
	fromBinary ("vcgt", size) =
		fmap (first ElemVCGT) . fromBinary size
	fromBinary ("ndin", size) =
--		fmap (first ElemNDIN) . fromBinary size
		fmap (first $ ElemOthers "ndin") . fromBinary (replicate size ())
	fromBinary (typ, size) =
		fmap (first $ ElemOthers typ) . fromBinary (replicate size ())
	toBinary (_, size) (ElemCurv dat) = toBinary size dat
	toBinary (_, size) (ElemData dat) = toBinary size dat
	toBinary (_, size) (ElemMFT2 dat) = toBinary size dat
	toBinary (_, size) (ElemMAB  dat) = toBinary size dat
	toBinary (_, size) (ElemMBA  dat) = toBinary size dat
	toBinary (_, size) (ElemXYZ dat) = toBinary size dat
	toBinary (_, size) (ElemChad dat) = toBinary size dat
	toBinary (_, size) (ElemText dat) = toBinary size dat
	toBinary (_, size) (ElemDesc dat) = toBinary size dat
	toBinary (_, size) (ElemMluc dat) = toBinary size dat
	toBinary (_, size) (ElemMmod dat) = toBinary size dat
	toBinary (_, size) (ElemPara dat) = toBinary size dat
	toBinary (_, size) (ElemVCGT dat) = toBinary size dat
	toBinary (_, size) (ElemNDIN dat) = toBinary size dat
	toBinary (_, size) (ElemOthers _ dat) = toBinary (replicate size ()) dat

[binary|

Curv deriving Show

arg :: Int

4{UInt32Number_}: num_curv
replicate num_curv 2{[UInt16Number]}: body_curv

|]

[binary|

Data deriving Show

arg :: Int

4: type_data
-- ((), Just (arg - 4)){String}: body_data
(type_data, arg - 4){DataBody}: body_data

|]

data DataBody = BinData [Word8] | ASCIIData String deriving Show

instance Field DataBody where
	type FieldArgument DataBody = (Int, Int)
	fromBinary (0, n) b = first ASCIIData <$> fromBinary (replicate n ()) b
	fromBinary (1, n) b = first BinData <$> fromBinary (replicate n ()) b
	fromBinary _ _ = error "bad data type"
	toBinary (_, n) (ASCIIData ad) = toBinary (replicate n ()) ad
	toBinary (_, n) (BinData bd) = toBinary (replicate n ()) bd

[binary|

MFT2 deriving Show

arg :: Int

1{UInt8Number}: input_num_mft2
1{UInt8Number}: output_num_mft2
1{UInt8Number}: clut_num_mft2
1: 0
{Matrix33}: matrix_mft2
2{UInt16Number}: input_table_n_mft2
2{UInt16Number}: output_table_n_mft2
replicate (input_table_n_mft2 * input_num_mft2) 2{[UInt16Number]}: input_table_mft2
replicate (clut_num_mft2 ^ input_num_mft2 * output_num_mft2) 2{[UInt16Number]}:
	clut_table_mft2
replicate (output_table_n_mft2 * output_num_mft2) 2{[UInt16Number]}:
	output_table_mft2

|]

[binary|

Matrix33 deriving Show

{S15Fixed16Number}: e1_matrix33
{S15Fixed16Number}: e2_matrix33
{S15Fixed16Number}: e3_matrix33
{S15Fixed16Number}: e4_matrix33
{S15Fixed16Number}: e5_matrix33
{S15Fixed16Number}: e6_matrix33
{S15Fixed16Number}: e7_matrix33
{S15Fixed16Number}: e8_matrix33
{S15Fixed16Number}: e9_matrix33

|]

type MBA_ = MAB_

data MBA = MBA {
	bCurvsMba :: [Body],
	matrixMba :: Maybe Matrix33,
	mCurvsMba :: [Body],
	clutMba :: Maybe MAB_CLUT,
	aCurvsMba :: [Body]
 } deriving Show

instance Field MBA where
	type FieldArgument MBA = Int
	fromBinary n bin = do
		(ret, rest) <- fromBinary n bin
		ret' <- mbaToMba ret
		return (ret', rest)
	toBinary n mba = toBinary n =<< mbaToMba_ mba

mbaToMba_ :: (Monad m, Applicative m) => MBA -> m MBA_
mbaToMba_ mba = do
	bcurvs <- to4Bytes <$> toBinary (repeat $ error "hoge") (bCurvsMba mba)
	matrix <- maybe (return "")
		(fmap to4Bytes . toBinary (error "hage")) $ matrixMba mba
	mcurvs <- to4Bytes <$> toBinary (repeat $ error "boke") (mCurvsMba mba)
	clut <- maybe (return "") (fmap to4Bytes . toBinary
		(length $ bCurvsMba mba, length $ aCurvsMba mba)) $ clutMba mba
	acurvs <- to4Bytes <$> toBinary (repeat $ error "kasu") (aCurvsMba mba)
	return MAB_ {
		b_offset_mab = if null bcurvs then 0 else 32,
		matrix_offset_mab = if null matrix then 0 else 32 + length bcurvs,
		m_offset_mab = if null mcurvs then 0 else 32 + length bcurvs +
			length matrix,
		clut_offset_mab = if null clut then 0 else 32 + length bcurvs +
			length matrix + length mcurvs,
		a_offset_mab = if null acurvs then 0 else 32 + length bcurvs +
			length matrix + length mcurvs + length clut,
		input_num_mab_ = length $ bCurvsMba mba,
		output_num_mab_ = length $ aCurvsMba mba,
		body_mab = bcurvs ++ matrix ++ mcurvs ++ clut ++ acurvs
	 }

mbaToMba :: (Monad m, Applicative m) => MBA_ -> m MBA
mbaToMba mab_ = do
--	(ret, _) <- fromBinary (output_num_mab mab_) $
	(bcurvs, _) <- fromBinary (replicate (input_num_mab_ mab_) (error "take")) $
		snd $ getBytes (b_offset_mab mab_ - 32) $ body_mab mab_
	(matrix, _) <- if matrix_offset == 0 then return (Nothing, undefined)
		else first Just <$>
			fromBinary () (snd $ getBytes (matrix_offset - 32) $
				body_mab mab_)
	(mcurvs, _) <- if m_offset == 0 then return ([], undefined) else
		fromBinary (replicate (input_num_mab_ mab_) undefined)
			(snd $ getBytes (m_offset_mab mab_ - 32) $ body_mab mab_)
	(clut, _) <- if clut_offset == 0 then return (Nothing, undefined) else
		first Just <$> fromBinary (input_num, output_num)
			(snd $ getBytes (clut_offset - 32) $ body_mab mab_)
	(acurvs, _) <- if a_offset == 0 then return ([], undefined) else
		fromBinary (replicate output_num undefined)
			(snd $ getBytes (a_offset - 32) $ body_mab mab_)
	return $ MBA bcurvs matrix mcurvs clut acurvs
	where
	matrix_offset = matrix_offset_mab mab_
	m_offset = m_offset_mab mab_
	clut_offset = clut_offset_mab mab_
	input_num = input_num_mab_ mab_
	output_num = output_num_mab_ mab_
	a_offset = a_offset_mab mab_

data MAB = MAB {
--	mab__mab :: MAB_,
	bCurvsMab :: [Body],
	matrixMab :: Maybe Matrix33,
	mCurvsMab :: [Body],
	clutMab :: Maybe MAB_CLUT,
	aCurvsMab  :: [Body]
 } deriving Show

instance Field MAB where
	type FieldArgument MAB = Int
	fromBinary n bin = do
		(ret, rest) <- fromBinary n bin
		ret' <- mabToMab ret
		return (ret', rest)
	toBinary n mab = toBinary n =<< mabToMab_ mab

mabToMab_ :: (Monad m, Applicative m) => MAB -> m MAB_
mabToMab_ mab = do
	bcurvs <- to4Bytes <$> toBinary (repeat $ error "hokeru") (bCurvsMab mab)
	matrix <- maybe (return "")
		(fmap to4Bytes . toBinary (error "bokeru")) $ matrixMab mab
	mcurvs <- to4Bytes <$> toBinary (repeat $ error "kokeru") (mCurvsMab mab)
	clut <- maybe (return "")
		(fmap to4Bytes . toBinary
				(length $ aCurvsMab mab, length $ bCurvsMab mab)) $
			clutMab mab
	acurvs <- to4Bytes <$> toBinary (repeat $ error "mumumu") (aCurvsMab mab)
	return MAB_ {
		b_offset_mab = if null bcurvs then 0 else 32,
		matrix_offset_mab = if null matrix then 0 else 32 + length bcurvs,
		m_offset_mab = if null mcurvs then 0 else 32 + length bcurvs +
			length matrix,
		clut_offset_mab = if null clut then 0 else 32 + length bcurvs +
			length matrix + length mcurvs,
		a_offset_mab = if null acurvs then 0 else 32 + length bcurvs +
			length matrix + length mcurvs + length clut,
		input_num_mab_ = length $ aCurvsMab mab,
		output_num_mab_ = length $ bCurvsMab mab,
		body_mab = bcurvs ++ matrix ++ mcurvs ++ clut ++ acurvs
	 }

to4Bytes :: String -> String
to4Bytes str
	| l == 0 = str
	| otherwise = str ++ replicate (4 - l) '\0'
	where
	l = length str `mod` 4

mabToMab :: (Monad m, Applicative m) => MAB_ -> m MAB
mabToMab mab_ = do
--	(ret, _) <- fromBinary (output_num_mab mab_) $
	(bcurvs, _) <- fromBinary (replicate (output_num_mab_ mab_) undefined) $
		snd $ getBytes (b_offset_mab mab_ - 32) $ body_mab mab_
	let	matrix_offset = matrix_offset_mab mab_
		m_offset = m_offset_mab mab_
		clut_offset = clut_offset_mab mab_
		input_num = input_num_mab_ mab_
		output_num = output_num_mab_ mab_
		a_offset = a_offset_mab mab_
	(matrix, _) <- if matrix_offset == 0 then return (Nothing, undefined)
		else first Just <$>
			fromBinary () (snd $ getBytes (matrix_offset - 32) $
				body_mab mab_)
	(mcurvs, _) <- if m_offset == 0 then return ([], undefined) else
		fromBinary (replicate (output_num_mab_ mab_) undefined)
			(snd $ getBytes (m_offset_mab mab_ - 32) $ body_mab mab_)
	(clut, _) <- if clut_offset == 0 then return (Nothing, undefined) else
		first Just <$> fromBinary (input_num, output_num)
			(snd $ getBytes (clut_offset - 32) $ body_mab mab_)
	(acurvs, _) <- if a_offset == 0 then return ([], undefined) else
		fromBinary (replicate input_num undefined)
			(snd $ getBytes (a_offset - 32) $ body_mab mab_)
	return $ MAB bcurvs matrix mcurvs clut acurvs

[binary|

MAB_ deriving Show

arg :: Int

1{UInt8Number}: input_num_mab_
1{UInt8Number}: output_num_mab_
2: 0
4{UInt32Number_}: b_offset_mab
4{UInt32Number_}: matrix_offset_mab
4{UInt32Number_}: m_offset_mab
4{UInt32Number_}: clut_offset_mab
4{UInt32Number_}: a_offset_mab
replicate (arg - 24) (){String}: body_mab

|]

[binary|

MAB_CLUT deriving Show

arg :: (Int, Int)

replicate 16 1{[Int]}: nums_mab_clut
1: byte_num_mab_clut
3: 0
replicate (product (take (fst arg) nums_mab_clut) * snd arg) byte_num_mab_clut
	{[UInt8Or16Number]}: body_mab_clut

|]



data Unicode16BE = Unicode16BE String deriving Show

instance Field Unicode16BE where
	type FieldArgument Unicode16BE = Int
	fromBinary n b = first (Unicode16BE . Text.unpack . decodeUtf16BE) <$> fromBinary n b
	toBinary n (Unicode16BE t) = toBinary n $ encodeUtf16BE $ Text.pack t

data MLUC = MLUC {
	mlucBody :: [MLUC_RECORD]
 } deriving Show

data MLUC_RECORD = MLUC_RECORD {
	langMLUC :: String,
	countryMLUC :: String,
	bodyMLUC :: String
 } deriving Show

instance Field MLUC where
	type FieldArgument MLUC = Int
	fromBinary n bs = do
		(ret, rest) <- fromBinary n bs
		ret' <- mlucPreToMluc ret
		return (ret', rest)
	toBinary n = toBinary n . mlucToMlucPre

mlucToMlucPre :: MLUC -> MLUC_Pre
mlucToMlucPre (MLUC mlucrs) =
	uncurry (MLUC_Pre $ length mlucrs) (getRecords (length mlucrs) mlucrs)

getRecords :: Int -> [MLUC_RECORD] -> ([MLUC_RECORD2], BS.ByteString)
getRecords num mlucrs = let
	(langs, dats) = unzip $ map fromMLUCRecord mlucrs
	offsets = getOffsets (12 * num + 16) dats
	lens = map BS.length dats
	body = getMLUCBody dats in
	(zipWith3 (uncurry MLUC_RECORD2) langs lens offsets, body)
	

getMLUCBody :: [BS.ByteString] -> BS.ByteString
getMLUCBody = BS.concat

getOffsets :: Int -> [BS.ByteString] -> [Int]
getOffsets _ [] = []
getOffsets n0 (dat : dats) = n0 : getOffsets (n0 + BS.length dat) dats

fromMLUCRecord :: MLUC_RECORD -> ((String, String), BS.ByteString)
fromMLUCRecord (MLUC_RECORD lang country dat) =
	((lang, country), encodeUtf16BE $ Text.pack dat)

-- getOffsets :: Int -> [MLUC_RECORD] -> [Int]
-- getOffsets n mlucr
--	offset = offset_MLUC_ rec - 12 * n - 16

mlucPreToMluc :: (Monad m, Applicative m) => MLUC_Pre -> m MLUC
mlucPreToMluc MLUC_Pre {
	num_MLUC_ = num,
	record_MLUC_ = records,
	body_MLUC_ = bodys
 } = MLUC <$> mapM (flip (mkMLUCRecord num) bodys) records

mkMLUCRecord :: (Monad m, Applicative m) =>
	Int -> MLUC_RECORD2 -> BS.ByteString -> m MLUC_RECORD
mkMLUCRecord n rec bs =
	MLUC_RECORD (lang_MLUC_ rec) (country_MLUC_ rec) <$> getEachMLUC n rec bs

getEachMLUC :: (Monad m, Applicative m) =>
	Int -> MLUC_RECORD2 -> BS.ByteString -> m String
getEachMLUC n rec bs = do
	(ret, _) <- fromBinary len $ snd $ getBytes offset bs
	return $ Text.unpack $ decodeUtf16BE ret
	where
	len = len_MLUC_ rec
	offset = offset_MLUC_ rec - 12 * n - 16

[binary|

MLUC_Pre deriving Show

arg :: Int

4: num_MLUC_
4: 12
replicate num_MLUC_ (){[MLUC_RECORD2]}: record_MLUC_
-- ((), Just (arg - 12 * num_MLUC_ - 8)){String}: body_MLUC_
-- arg - 12 * num_MLUC_ - 8{Unicode16BE}: body_MLUC_
arg - 12 * num_MLUC_ - 8{BS.ByteString}: body_MLUC_

|]

[binary|

MLUC_RECORD2 deriving Show

replicate 2 (){String}: lang_MLUC_
replicate 2 (){String}: country_MLUC_
4: len_MLUC_
4: offset_MLUC_

|]
