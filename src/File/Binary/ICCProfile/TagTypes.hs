{-# LANGUAGE QuasiQuotes, TypeFamilies, EmptyDataDecls, PatternGuards #-}

module File.Binary.ICCProfile.TagTypes (
	Body(..),
	Elem(..),
	Curv(..),
	Data(..),
	MFT2(..),
	MAB(..),
	MBA(..),
	MAB_(..),
	MAB_CLUT(..),
	MLUC_(..),
	Unicode16BE(..),
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
import Data.Text.Encoding
import Data.Text (pack, unpack)
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

arg :: Int

((), Just 4){String}: data_type
4: 0
(data_type, arg - 8){Elem}: data_body
getPadding $ getSize arg data_body: 0 -- padd

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
	| ElemMluc MLUC_
	| ElemText Text2
	| ElemXYZ XYZ2
	| ElemDesc Desc
	| ElemChad CHAD2
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
		fmap (first $ ElemOthers "ndin") . fromBinary ((), Just size)
	fromBinary (typ, size) =
		fmap (first $ ElemOthers typ) . fromBinary ((), Just size)
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
	toBinary (_, size) (ElemOthers _ dat) = toBinary ((), Just size) dat

[binary|

Curv deriving Show

arg :: Int

4{UInt32Number_}: num_curv
(2, Just num_curv){[UInt16Number]}: body_curv

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
	fromBinary (0, n) b = first ASCIIData <$> fromBinary ((), Just n) b
	fromBinary (1, n) b = first BinData <$> fromBinary ((), Just n) b
	fromBinary _ _ = error "bad data type"
	toBinary (_, n) (ASCIIData ad) = toBinary ((), Just n) ad
	toBinary (_, n) (BinData bd) = toBinary ((), Just n) bd

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
(2, Just $ input_table_n_mft2 * input_num_mft2){[UInt16Number]}: input_table_mft2
(2, Just $ clut_num_mft2 ^ input_num_mft2 * output_num_mft2){[UInt16Number]}: clut_table_mft2
(2, Just $ output_table_n_mft2 * output_num_mft2){[UInt16Number]}: output_table_mft2

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
	aCurvsMba  :: [Body]
 } deriving Show

data MAB = MAB {
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

instance Field MBA where
	type FieldArgument MBA = Int
	fromBinary n bin = do
		(ret, rest) <- fromBinary n bin
		ret' <- mbaToMba ret
		return (ret', rest)

mabToMab_ :: (Monad m, Applicative m) => MAB -> m MAB_
mabToMab_ mab = do
	bcurvs <- to4Bytes <$> toBinary (undefined, undefined) (bCurvsMab mab)
	matrix <- maybe (return "")
		(fmap to4Bytes . toBinary undefined) $ matrixMab mab
	mcurvs <- to4Bytes <$> toBinary (undefined, undefined) (mCurvsMab mab)
	clut <- maybe (return "")
		(fmap to4Bytes . toBinary undefined) $ clutMab mab
	acurvs <- to4Bytes <$> toBinary (undefined, undefined) (aCurvsMab mab)
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
mabToMab mab_@MAB_ {
	matrix_offset_mab = matrix_offset,
	m_offset_mab = m_offset,
	clut_offset_mab = clut_offset,
	input_num_mab_ = input_num,
	output_num_mab_ = output_num,
	a_offset_mab = a_offset
 } = do
--	(ret, _) <- fromBinary (output_num_mab mab_) $
	(bcurvs, _) <- fromBinary (undefined, Just $ output_num_mab_ mab_) $
		snd $ getBytes (b_offset_mab mab_ - 32) $ body_mab mab_
	(matrix, _) <- if matrix_offset == 0 then return (Nothing, undefined)
		else first Just <$>
			fromBinary () (snd $ getBytes (matrix_offset - 32) $
				body_mab mab_)
	(mcurvs, _) <- if m_offset == 0 then return ([], undefined) else
		fromBinary (undefined, Just $ output_num_mab_ mab_)
			(snd $ getBytes (m_offset_mab mab_ - 32) $ body_mab mab_)
	(clut, _) <- if clut_offset == 0 then return (Nothing, undefined) else
		first Just <$> fromBinary (input_num, output_num)
			(snd $ getBytes (clut_offset - 32) $ body_mab mab_)
	(acurvs, _) <- if a_offset == 0 then return ([], undefined) else
		fromBinary (undefined, Just input_num)
			(snd $ getBytes (a_offset - 32) $ body_mab mab_)
	return $ MAB bcurvs matrix mcurvs clut acurvs

mbaToMba :: (Monad m, Applicative m) => MBA_ -> m MBA
mbaToMba mab_ = do
--	(ret, _) <- fromBinary (output_num_mab mab_) $
	(bcurvs, _) <- fromBinary (undefined, Just $ input_num_mab_ mab_) $
		snd $ getBytes (b_offset_mab mab_ - 32) $ body_mab mab_
	(matrix, _) <- if matrix_offset == 0 then return (Nothing, undefined)
		else first Just <$>
			fromBinary () (snd $ getBytes (matrix_offset - 32) $
				body_mab mab_)
	(mcurvs, _) <- if m_offset == 0 then return ([], undefined) else
		fromBinary (undefined, Just $ input_num_mab_ mab_)
			(snd $ getBytes (m_offset_mab mab_ - 32) $ body_mab mab_)
	(clut, _) <- if clut_offset == 0 then return (Nothing, undefined) else
		first Just <$> fromBinary (input_num, output_num)
			(snd $ getBytes (clut_offset - 32) $ body_mab mab_)
	(acurvs, _) <- if a_offset == 0 then return ([], undefined) else
		fromBinary (undefined, Just output_num)
			(snd $ getBytes (a_offset - 32) $ body_mab mab_)
	return $ MBA bcurvs matrix mcurvs clut acurvs
	where
	matrix_offset = matrix_offset_mab mab_
	m_offset = m_offset_mab mab_
	clut_offset = clut_offset_mab mab_
	input_num = input_num_mab_ mab_
	output_num = output_num_mab_ mab_
	a_offset = a_offset_mab mab_

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
((), Just $ arg - 24){String}: body_mab

|]

[binary|

MAB_CLUT deriving Show

arg :: (Int, Int)

(1, Just 16){[Int]}: nums_mab_clut
1: byte_num_mab_clut
3: 0
(byte_num_mab_clut, Just $ product (take (fst arg) nums_mab_clut) * snd arg)
	{[UInt8Or16Number]}: body_mab_clut

|]

data Unicode16BE = Unicode16BE String deriving Show

instance Field Unicode16BE where
	type FieldArgument Unicode16BE = Int
	fromBinary n b = first (Unicode16BE . unpack . decodeUtf16BE) <$> fromBinary n b
	toBinary n (Unicode16BE t) = toBinary n $ encodeUtf16BE $ pack t

-- instance Field MLUC where

data MLUC = MLUC {
	mlucBody :: [MLUC_RECORD]
 } deriving Show

data MLUC_RECORD = MLUC_RECORD deriving Show

mlucToMluc :: MLUC_ -> MLUC
mlucToMluc mluc_ = MLUC {}

getEachMLUC :: (Monad m, Applicative m) =>
	Int -> MLUC_RECORD2 -> BS.ByteString -> m String
getEachMLUC n rec bs = do
	(ret, _) <- fromBinary len $ snd $ getBytes offset bs
	return $ unpack $ decodeUtf16BE ret
	where
	len = len_MLUC_ rec
	offset = offset_MLUC_ rec - 12 * n - 8

[binary|

MLUC_ deriving Show

arg :: Int

4: num_MLUC_
4: 12
((), Just num_MLUC_){[MLUC_RECORD2]}: record_MLUC_
-- ((), Just (arg - 12 * num_MLUC_ - 8)){String}: body_MLUC_
arg - 12 * num_MLUC_ - 8{Unicode16BE}: body_MLUC_

|]

[binary|

MLUC_RECORD2 deriving Show

((), Just 2){String}: lang_MLUC_
((), Just 2){String}: country_MLUC_
4: len_MLUC_
4: offset_MLUC_

|]
