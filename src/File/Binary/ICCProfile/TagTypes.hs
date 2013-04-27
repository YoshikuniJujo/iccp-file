{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module File.Binary.ICCProfile.TagTypes (
	Body(..),
	Elem(..),
	Curv(..),
	Data(..),
	MFT2(..),
	MAB(..),
	MAB_(..),
	module File.Binary.ICCProfile.TagTypes_yet
) where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()

import File.Binary.ICCProfile.TagTypes_yet

import Control.Applicative
import Control.Arrow

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
	| ElemText Text2
	| ElemXYZ XYZ2
	| ElemDesc Desc
	| ElemChad CHAD2
	| ElemMluc MLUC2
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

4: num_curv
(2, Just num_curv){[Int]}: body_curv

|]

[binary|

Data deriving Show

arg :: Int

4: type_data
((), Just (arg - 4)){String}: body_data

|]

[binary|

MFT2 deriving Show

arg :: Int

1: input_num_mft2
1: output_num_mft2
1: clut_num_mft2
1: 0
{Matrix33}: matrix_mft2
2: input_table_n_mft2
2: output_table_n_mft2
(2, Just $ input_table_n_mft2 * input_num_mft2){[Int]}: input_table_mft2
(2, Just $ clut_num_mft2 ^ input_num_mft2 * output_num_mft2){[Int]}: clut_table_mft2
(2, Just $ output_table_n_mft2 * output_num_mft2){[Int]}: output_table_mft2

|]

[binary|

Matrix33 deriving Show

4: e1_matrix33
4: e2_matrix33
4: e3_matrix33
4: e4_matrix33
4: e5_matrix33
4: e6_matrix33
4: e7_matrix33
4: e8_matrix33
4: e9_matrix33

|]

data MAB = MAB {
	mab__mab :: MAB_,
--	b_curvs :: BodyList
	b_curvs_mab :: [Body],
	matrix_mab :: Maybe Matrix33,
	m_curvs_mab :: Maybe [Body] -- ,
--	clut_mab :: MAB_CLUT
 } deriving Show

instance Field MAB where
	type FieldArgument MAB = Int
	fromBinary n bin = do
		(ret, rest) <- fromBinary n bin
		ret' <- mab_ToMab ret
		return (ret', rest)
	toBinary n (MAB mab_ _ _ _) = toBinary n mab_

mab_ToMab :: (Monad m, Functor m) => MAB_ -> m MAB
mab_ToMab mab_ = do
--	(ret, _) <- fromBinary (output_num_mab mab_) $
	(bcurvs, _) <- fromBinary (undefined, Just $ output_num_mab mab_) $
		snd $ getBytes (b_offset_mab mab_ - 32) $ body_mab mab_
	let	matrix_offset = matrix_offset_mab mab_
		m_offset = m_offset_mab mab_
	(matrix, _) <- if matrix_offset == 0 then return (Nothing, undefined)
		else first Just <$>
			fromBinary () (snd $ getBytes (matrix_offset - 32) $
				body_mab mab_)
	(mcurvs, _) <- if m_offset == 0 then return (Nothing, undefined) else
		first Just <$> fromBinary (undefined, Just $ output_num_mab mab_)
			(snd $ getBytes (m_offset_mab mab_ - 32) $ body_mab mab_)
--	(clut, _)
	return $ MAB mab_ bcurvs matrix mcurvs

[binary|

MAB_ deriving Show

arg :: Int

1: input_num_mab
1: output_num_mab
2: 0
4: b_offset_mab
4: matrix_offset_mab
4: m_offset_mab
4: clut_offset_mab
4: a_offset_mab
((), Just $ arg - 24){String}: body_mab

|]

[binary|

MAB_CLUT deriving Show

arg :: (Int, Int)

(1, Just 16){[Int]}: nums_mab_clut
1: byte_num_mab_clut
3: 0
(byte_num_mab_clut, Just $ product (take (fst arg) nums_mab_clut) * snd arg):
	body_mab_clut

|]


