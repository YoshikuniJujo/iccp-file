{-# LANGUAGE FlexibleInstances #-}

module Short (short) where

import File.Binary.ICCProfile
import File.Binary.ICCProfile.TagTypes
import Data.List

dotdot :: Int -> Int -> String -> String
dotdot i t str
	| length str <= i + t = str
	| otherwise = take i str ++ " ... " ++ reverse (take t $ reverse str)

class Short a where
	short :: a -> String

instance Short a => Short [a] where
	short xs = "[" ++ intercalate ", " (map short xs) ++ "]"

instance Short a => Short (Maybe a) where
	short (Just x) = "Just " ++ short x
	short _ = "Nothing"

instance Short a => Short (String, a) where
	short (s, y) = "(" ++ s ++ ", " ++ short y ++ ")"

instance Short ICCP where
	short iccp ="ICCP {" ++
		"size = " ++ show size ++ ", " ++
		"type = " ++ show typ ++ ", " ++
		"version = " ++ show vmajor ++ "." ++ show vminor ++ ", " ++
		"device_class = " ++ show dev_class ++ ", " ++
		"color_space = " ++ show clrspace ++ ", " ++
		"connection_space = " ++ show cnspace ++ ", " ++
		"create_time = " ++ show ctime ++ ", " ++
		"target_platform = " ++ show tplat ++ ", " ++
		"flags = " ++ show pflags ++ ", " ++
		"is_embeded = " ++ show iembed ++ ", " ++
		"only_embeded = " ++ show oembed ++ ", " ++
		"device_manufacturer = " ++ show dmanu ++ ", " ++
		"device_model = " ++ show dmod ++ ", " ++
		"device_attributes = " ++ show dattr ++ ", " ++
		"device_trans = " ++ show dtran ++ ", " ++
		"device_matte = " ++ show dmatt ++ ", " ++
		"device_nega = " ++ show dneg ++ ", " ++
		"device_not_color = " ++ show dnc ++ ", " ++
		"rendering_intent = " ++ show ri ++ ", " ++
		"illuminant_value = " ++ show iv ++ ", " ++
		"profile_creator = " ++ show pc ++ ", " ++
		"profile_identifier = " ++ show pint ++ ", " ++
		"tag_count = " ++ show tc ++ ", " ++
		"tags = " ++ show ts ++ ", " ++
		"bodys = " ++ short bs ++
		"}"
		where
		ICCP size typ vmajor vminor dev_class clrspace cnspace ctime tplat
			pflags iembed oembed dmanu dmod dattr dtran dmatt dneg dnc
			ri iv pc pint tc ts bs = iccp

instance Short Body where
	short dat = "Body " ++
--		show (padd dat) ++ " " ++
		"(" ++ short (data_body dat) ++ ")"

instance Short Elem where
	short (ElemCurv curv) = "ElemCurv " ++ "(" ++ short curv ++ ")"
	short (ElemData dat ) = "ElemData " ++ "(" ++ short dat  ++ ")"
	short (ElemMFT2 mft2) = "ElemMFT2 " ++ "(" ++ short mft2 ++ ")"
	short (ElemMAB  mab ) = "ElemMAB "  ++ "(" ++ short mab  ++ ")"
	short (ElemMBA  mba ) = "ElemMBA "  ++ "(" ++ short mba  ++ ")"
	short (ElemMluc mluc) = "ElemMluc " ++ "(" ++ short mluc ++ ")"
	short (ElemMmod mmod) = "ElemMmod " ++ "(" ++ short mmod ++ ")"
	short (ElemText txt) = "ElemText " ++ "(" ++ short txt ++ ")"
	short (ElemVCGT vcgt) = "ElemVCGT " ++ "(" ++ dotdot 100 100 (show vcgt) ++ ")"
	short (ElemOthers t b) =
		"ElemOthers " ++ t ++ "(" ++ dotdot 10 10 (show b) ++ ")"
	short elm = show elm

instance Short Curv where
	short crv = "Curv " ++ "{" ++
		"num_curv = " ++ show (num_curv crv) ++ ", " ++
		"curv = " ++ dotdot 20 20 (show $ body_curv crv) ++
		"}"

instance Short Data where
	short = dotdot 40 30 . show

instance Short MFT2 where
	short mft2 = "MFT2 " ++ "{" ++
		"i = " ++ show (input_num_mft2 mft2) ++ ", " ++
		"o = " ++ show (output_num_mft2 mft2) ++ ", " ++
		"g = " ++ show (clut_num_mft2 mft2) ++ ", " ++
		"matrix = " ++ show (matrix_mft2 mft2) ++ ", " ++
{-
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
-}
		"n = " ++ show (input_table_n_mft2 mft2) ++ ", " ++
		"m = " ++ show (output_table_n_mft2 mft2) ++ ", " ++
		"input_table = " ++ dotdot 10 20 (show $ input_table_mft2 mft2) ++
			", " ++
		"clut_table = " ++ dotdot 60 30 (show $ clut_table_mft2 mft2) ++
			", " ++
		"output_table = " ++ dotdot 10 20 (show $ output_table_mft2 mft2) ++
		"}"

instance Short MAB where
	short mab_ = "MAB " ++ "{" ++
		"bcurvs = " ++ short bcs ++ ", " ++
		"matrix = " ++ show matrix ++ ", " ++
		"mcurvs = " ++ short mcs ++ ", " ++
		"clut = " ++ short clut ++ ", " ++
		"acurvs = " ++ short acs ++
		"}"
		where
		MAB bcs matrix mcs clut acs = mab_

instance Short MBA where
	short mba_ = "MBA " ++ "{" ++
		"bcurvs = " ++ short bcs ++ ", " ++
		"matrix = " ++ show matrix ++ ", " ++
		"mcurvs = " ++ short mcs ++ ", " ++
		"clut = " ++ short clut ++ ", " ++
		"acurvs = " ++ short acs ++
		"}"
		where
		MBA bcs matrix mcs clut acs = mba_

instance Short MAB_CLUT where
	short mc = "MAB_CLUT{" ++
		"nums = " ++ show nums ++ ", " ++
		"byte = " ++ show byte ++ ", " ++
		"body = " ++ dotdot 100 100 (show body) ++
		"}"
		where
		MAB_CLUT nums byte body = mc

instance Short Text2 where
	short t = show $ dotdot 10 10 $ text t

instance Short MLUC where
	short mluc = "MLUC " ++ short (mlucBody mluc)

instance Short MLUC_RECORD where
	short mlucr = "MLUC_RECORD {" ++
		"lang = " ++ langMLUC mlucr ++ ", " ++
		"country = " ++ countryMLUC mlucr ++ ", " ++
		"body = " ++ bodyMLUC mlucr ++ "}"

instance Short MLUC_Pre where
	short mluc = "(MLUC2 " ++
		show (num_MLUC_ mluc) ++ " " ++
		dotdot 20 20 (show $ record_MLUC_ mluc) ++ " " ++
		dotdot 40 40 (show $ body_MLUC_ mluc) ++ ")"

instance Short MMOD2 where
	short mmod = "(MMOD2 " ++
		dotdot 10 10 (show $ body_MMOD2 mmod) ++ ")"
