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

instance Short Data where
	short dat = "Data (" ++ short (data_body dat) ++ ")"

instance Short Elem where
	short (ElemMluc mluc) = "ElemMluc " ++ "(" ++ short mluc ++ ")"
	short (ElemMmod mmod) = "ElemMmod " ++ "(" ++ short mmod ++ ")"
	short (ElemText txt) = "ElemText " ++ "(" ++ short txt ++ ")"
	short (ElemCurv curv) = "ElemCurv " ++ "(" ++ short curv ++ ")"
	short elm = show elm

instance Short Text2 where
	short t = show $ dotdot 10 10 $ text t

instance Short Curv2 where
	short = dotdot 20 20 . show

instance Short MLUC2 where
	short mluc = "(MLUC2 " ++
		show (num_MLUC2 mluc) ++ " " ++
		dotdot 20 20 (show $ record_MLUC2 mluc) ++ " " ++
		dotdot 40 40 (show $ body_MLUC2 mluc) ++ ")"

instance Short MMOD2 where
	short mmod = "(MMOD2 " ++
		dotdot 10 10 (show $ body_MMOD2 mmod) ++ ")"
