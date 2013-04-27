{-# LANGUAGE FlexibleInstances #-}

module Short (short) where

import File.Binary.ICCProfile
import File.Binary.ICCProfile.TagTypes
import Data.List

dotdot :: Int -> Int -> String -> String
dotdot i t str = take i str ++ " ... " ++ reverse (take t $ reverse str)

class Short a where
	short :: a -> String

instance Short a => Short [a] where
	short xs = "[" ++ intercalate ", " (map short xs) ++ "]"

instance Short a => Short (String, a) where
	short (s, y) = "(" ++ s ++ ", " ++ short y ++ ")"

instance Short Body where
	short dat = "Body (" ++ short (data_body dat) ++ ")"

instance Short Elem where
	short (ElemCurv curv) = "ElemCurv " ++ "(" ++ short curv ++ ")"
	short (ElemData dat ) = "ElemData " ++ "(" ++ short dat  ++ ")"
	short (ElemMFT2 mft2) = "ElemMFT2 " ++ "(" ++ short mft2 ++ ")"
	short (ElemMAB  mab ) = "ElemMAB "  ++ "(" ++ short mab  ++ ")"
	short (ElemMluc mluc) = "ElemMluc " ++ "(" ++ short mluc ++ ")"
	short (ElemMmod mmod) = "ElemMmod " ++ "(" ++ short mmod ++ ")"
	short (ElemText txt) = "ElemText " ++ "(" ++ short txt ++ ")"
	short (ElemOthers t b) =
		"ElemOthers " ++ t ++ "(" ++ dotdot 10 10 (show b) ++ ")"
	short elm = show elm

instance Short Curv where
	short = dotdot 20 20 . show

instance Short Data where
	short = dotdot 40 30 . show

instance Short MFT2 where
	short mft2 = "MFT2 " ++ "{" ++
		"i = " ++ show (input_num_mft2 mft2) ++ ", " ++
		"o = " ++ show (output_num_mft2 mft2) ++ ", " ++
		"g = " ++ show (clut_num_mft2 mft2) ++ ", " ++
		"e[1-9] = " ++
			show (e1_mft2 mft2) ++ " " ++
			show (e2_mft2 mft2) ++ " " ++
			show (e3_mft2 mft2) ++ " " ++
			show (e4_mft2 mft2) ++ " " ++
			show (e5_mft2 mft2) ++ " " ++
			show (e6_mft2 mft2) ++ " " ++
			show (e7_mft2 mft2) ++ " " ++
			show (e8_mft2 mft2) ++ " " ++
			show (e9_mft2 mft2) ++ ", " ++
		"n = " ++ show (input_table_n_mft2 mft2) ++ ", " ++
		"m = " ++ show (output_table_n_mft2 mft2) ++ ", " ++
		"input_table = " ++ dotdot 10 20 (show $ input_table_mft2 mft2) ++
			", " ++
		"clut_table = " ++ dotdot 60 30 (show $ clut_table_mft2 mft2) ++
			", " ++
		"output_table = " ++ dotdot 10 20 (show $ output_table_mft2 mft2) ++
		"}"

instance Short MAB where
	short mab = "MAB " ++ "{" ++
		"i = " ++ show (input_num_mab mab) ++ ", " ++
		"o = " ++ show (output_num_mab mab) ++ ", " ++
		"b_offset = " ++ show (b_offset_mab mab) ++ ", " ++
		"matrix_offset = " ++ show (matrix_offset_mab mab) ++ ", " ++
		"clut_offset = " ++ show (clut_offset_mab mab) ++ ", " ++
		"a_offset = " ++ show (a_offset_mab mab) ++ ", " ++
		"body_mab = " ++ dotdot 20 20 (show $ body_mab mab) ++
		"}"

instance Short Text2 where
	short t = show $ dotdot 10 10 $ text t

instance Short MLUC2 where
	short mluc = "(MLUC2 " ++
		show (num_MLUC2 mluc) ++ " " ++
		dotdot 20 20 (show $ record_MLUC2 mluc) ++ " " ++
		dotdot 40 40 (show $ body_MLUC2 mluc) ++ ")"

instance Short MMOD2 where
	short mmod = "(MMOD2 " ++
		dotdot 10 10 (show $ body_MMOD2 mmod) ++ ")"
