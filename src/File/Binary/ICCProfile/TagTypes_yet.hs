{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module File.Binary.ICCProfile.TagTypes_yet where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()

[binary|

NDIN2 deriving Show

arg :: Int

(4, Just 9){[Int]}: hoge_NDIN2
(4, Just 3){[Int]}: hage_NDIN2
(2, Just 3){[Int]}: hige_NDIN2
(2, Just ((arg - 58) `div` 2)){[Int]}: body_NDIN2

|]

[binary|

VCGT2 deriving Show

arg :: Int

4: 0
2: hoge_VCGT2
2: hage_VCGT2
2: hige_VCGT2
(1, Just (arg - 10)){[Int]}: body_VCGT2
-- (2, Just (arg `div` 2 - 7)){[Int]}: body_VCGT2
-- {Int}: some
-- (arg - ((arg `div` 2) * 2)): some
-- 1: 0

|]

[binary|

Text2

arg :: Int

((), Just arg){String}: text

|]

instance Show Text2 where
	show = text

[binary|

XYZ2 deriving Show

arg :: Int

{XYZ}: xyz
-- 4: xyz_X
-- 4: xyz_Y
-- 4: xyz_Z

|]

[binary|

Desc deriving Show

arg :: Int

((), Just arg){String}: body_desc

|]

[binary|

CHAD2 deriving Show

arg :: Int

4: chad2_a0
4: chad2_a1
4: chad2_a2
4: chad2_a3
4: chad2_a4
4: chad2_a5
4: chad2_a6
4: chad2_a7
4: chad2_a8

|]

[binary|

MLUC2 deriving Show

arg :: Int

4: num_MLUC2
4: 12
((), Just num_MLUC2){[MLUC_RECORD2]}: record_MLUC2
((), Just (arg - 12 * num_MLUC2 - 8)){String}: body_MLUC2

|]

[binary|

MLUC_RECORD2 deriving Show

((), Just 2){String}: lang_MLUC2
((), Just 2){String}: country_MLUC2
4: len_MLUC2
4: offset_MLUC2

|]

[binary|

MMOD2

arg :: Int

((), Just arg){String}: body_MMOD2

|]

instance Show MMOD2 where
	show mmod = "(MMOD2 " ++
		show (body_MMOD2 mmod) ++ ")"

[binary|

Para2

arg :: Int

2: functype_Para2
2: 0
4: g_Para2
4: a_Para2
4: b_Para2
4: c_Para2
4: d_Para2

|]

instance Show Para2 where
	show para = "(Para2 " ++
		show (functype_Para2 para) ++ " " ++
		show (g_Para2 para) ++ " " ++
		show (a_Para2 para) ++ " " ++
		show (b_Para2 para) ++ " " ++
		show (c_Para2 para) ++ " " ++
		show (d_Para2 para) ++ ")"

[binary|

XYZ deriving Show

4: xyz_X
4: xyz_Y
4: xyz_Z

|]
