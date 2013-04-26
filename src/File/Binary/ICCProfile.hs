{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.ICCProfile (
	ICCP(..), Tag(..), readICCP, writeICCP,
	short, paddings, sizes,
	duplicate, fromElems, filePadding, tagTypes
) where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian (BitsInt)
import Control.Arrow
import Data.Monoid
import Data.List
import Data.Time

type ICCP_Data = (ICCP, [Element])

readICCP :: (Monad m, Functor m, Binary b) => b -> m ICCP_Data
readICCP bin = do
	(ret, _) <- fromBinary () bin
	elems <- mapM (($ bin) . getElement') $ tags ret
	return (ret, elems)

tagTypes :: (Monad m, Functor m, Binary b) => b -> m [String]
tagTypes bin = do
	(ret, _) <- fromBinary () bin
	tts <- mapM (($ bin) . getTagTypeName) $ tags ret
	return $ map tag_type_name tts

-- writeICCP :: (Monad m, Functor m, Binary b) => ICCP_Data -> m b
-- writeICCP :: ICCP_Data -> IO String
writeICCP :: (Monad m, Functor m) => ICCP_Data -> m String
writeICCP (ret, elems) = do
	let	dups = duplicate [] (tags ret)
		pads = paddings (deleteIndexes dups $ tags ret) ++ [0]
	bin <- toBinary () ret
	bins <- mapM (toBinary (error "bad values") . snd) $ deleteIndexes dups elems
--	let bins' = zipWith (\d p -> d `mappend` replicate p '\0') bins pads
	let	bins' = zipWith addPadding bins pads
	return $ mconcat (bin : bins') ++ replicate (filePadding ret) '\0'

filePadding :: ICCP -> Int
filePadding iccp = profile_size iccp -
	(tag_data_offset lastTag + tag_element_size lastTag)
	where
	lastTag = last $ tags iccp

fromElems :: (Monad m, Functor m) => [Element] -> m [String]
fromElems elems = do
	mapM (toBinary undefined . snd) elems

addPadding :: String -> Int -> String
addPadding d p
	| p >= 0 = d `mappend` replicate p '\0'
	| otherwise = take (length d + p) d

paddings :: [Tag] -> [Int]
paddings [] = error "bad"
paddings [_] = []
paddings (t0 : t1 : ts) =
	tag_data_offset t1 - tag_data_offset t0 - tag_element_size t0 :
	paddings (t1 : ts)

deleteIndexes :: [Int] -> [a] -> [a]
deleteIndexes [] xs = xs
deleteIndexes (n : ns) xs =
	take n xs ++ deleteIndexes (map (subtract (n + 1)) ns) (drop (n + 1) xs)

duplicate :: [Tag] -> [Tag] -> [Int]
duplicate _ [] = []
duplicate pre (t : post)
	| or $ map (sameOffsetSize t) pre = length pre : duplicate (t : pre) post
	| otherwise = duplicate (t : pre) post

sameOffsetSize :: Tag -> Tag -> Bool
sameOffsetSize (Tag _ offset1 size1) (Tag _ offset2 size2) =
	offset1 == offset2 && size1 == size2

sizes :: [Tag] -> [Int]
sizes [] = []
sizes (t : ts) = tag_element_size t : sizes ts

getElement' :: (Monad m, Functor m, Binary b) => Tag -> b -> m Element
getElement' t@(Tag tn _ _) str = do
	(d, _) <- getData t str
	return $ edata tn d

getTagTypeName :: (Monad m, Functor m, Binary b) => Tag -> b -> m TagType
getTagTypeName (Tag _ offset _) = fmap fst . fromBinary () . snd . getBytes offset

[binary|

TagType deriving Show

((), Just 4){String}: tag_type_name

|]

class Short a where
	short :: a -> String

instance Short a => Short [a] where
	short xs = "[" ++ intercalate ", " (map short xs) ++ "]"

instance Short a => Short (String, a) where
	short (s, y) = "(" ++ s ++ ", " ++ short y ++ ")"

[binary|

ICCP deriving Show

4: profile_size
((), Just 4){String}: cmm_type
1: profile_version_major
1: profile_version_minor
2: 0
((), Just 4){String}: profile_device_class
((), Just 4){String}: color_space_of_data
((), Just 4){String}: profile_connection_space
{UTCTime}: create_time
4: "acsp"
((), Just 4){String}: target_platform
2: profile_flags
14{BitsInt}: 0
{Bool}: is_embeded
{Bool}: only_embeded
4: device_manufacturer
4: device_model
4: device_attributes
28{BitsInt}: 0
{Bool}: device_not_color
{Bool}: device_nega
{Bool}: device_matte
{Bool}: device_trans
4: rendering_intent
{XYZ}: illuminant_value
((), Just 4){String}: profile_creator
((), Just 16){String}: profile_identifier
28{Integer}: 0
4: tag_count
((), Just tag_count){[Tag]}: tags

|]

[binary|

XYZ deriving Show

4: xyz_X
4: xyz_Y
4: xyz_Z

|]

instance Field UTCTime where
	type FieldArgument UTCTime = ()
	fromBinary () = fmap (first timeToUTCTime) . fromBinary ()
	toBinary () = toBinary () . utcTimeToTime

timeToUTCTime :: Time -> UTCTime
timeToUTCTime t = UTCTime (fromGregorian (fromIntegral y) mon d) $
	timeOfDayToTime (TimeOfDay h m $ fromIntegral s)
	where
	y = create_year t
	mon = create_month t
	d = create_day t
	h = create_hour t
	m = create_minuite t
	s = create_second t

utcTimeToTime :: UTCTime -> Time
utcTimeToTime (UTCTime day time) =
	Time (fromIntegral y) mon d h m (floor s)
	where
	(y, mon, d) = toGregorian day
	TimeOfDay h m s = timeToTimeOfDay time

[binary|

Time deriving Show

2: create_year
2: create_month
2: create_day
2: create_hour
2: create_minuite
2: create_second

|]

[binary|

Tag deriving Show

((), Just 4){String}: tag_signature
4: tag_data_offset
4: tag_element_size

|]

edata :: String -> Data -> Element
edata = (,)

type Element = (String, Data)

dotdot :: Int -> Int -> String -> String
dotdot i t str = take i str ++ " ... " ++ reverse (take t $ reverse str)

getData :: (Monad m, Functor m, Binary b) => Tag -> b -> m (Data, b)
getData (Tag _ offset size) = fromBinary size . snd . getBytes offset

[binary|

Data

arg :: Int

((), Just 4){String}: data_type
4: 0
(data_type, arg - 8){Elem}: data_body

|]

instance Show Data where
	show dat = "Data (" ++ show (data_body dat) ++ ")"
{-
	show dat = "(Data " ++
		show (data_type dat) ++ " " ++
		"(" ++ show (data_body dat) ++ "))"
-}

instance Short Data where
	short dat = "Data (" ++ short (data_body dat) ++ ")"

data Elem
	= ElemText Text2
	| ElemXYZ XYZ2
	| ElemDesc Desc
	| ElemCurv Curv2
	| ElemChad CHAD2
	| ElemMluc MLUC2
	| ElemMmod MMOD2
	| ElemPara Para2
	| ElemVCGT VCGT2
	| ElemNDIN NDIN2
	| ElemData String String
	deriving Show

instance Short Elem where
	short (ElemMluc mluc) = "ElemMluc " ++ "(" ++ short mluc ++ ")"
	short (ElemMmod mmod) = "ElemMmod " ++ "(" ++ short mmod ++ ")"
	short (ElemText txt) = "ElemText " ++ "(" ++ short txt ++ ")"
	short (ElemCurv curv) = "ElemCurv " ++ "(" ++ short curv ++ ")"
	short elm = show elm

instance Field Elem where
	type FieldArgument Elem = (String, Int)
	fromBinary ("XYZ ", size) =
		fmap (first ElemXYZ) . fromBinary size
	fromBinary ("curv", size) =
		fmap (first ElemCurv) . fromBinary size
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
		fmap (first $ ElemData "ndin") . fromBinary ((), Just size)
	fromBinary (typ, size) =
		fmap (first $ ElemData typ) . fromBinary ((), Just size)
	toBinary (_, size) (ElemXYZ dat) = toBinary size dat
	toBinary (_, size) (ElemCurv dat) = toBinary size dat
	toBinary (_, size) (ElemChad dat) = toBinary size dat
	toBinary (_, size) (ElemText dat) = toBinary size dat
	toBinary (_, size) (ElemDesc dat) = toBinary size dat
	toBinary (_, size) (ElemMluc dat) = toBinary size dat
	toBinary (_, size) (ElemMmod dat) = toBinary size dat
	toBinary (_, size) (ElemPara dat) = toBinary size dat
	toBinary (_, size) (ElemVCGT dat) = toBinary size dat
	toBinary (_, size) (ElemNDIN dat) = toBinary size dat
	toBinary (_, size) (ElemData _ dat) = toBinary ((), Just size) dat

[binary|

Text2

arg :: Int

((), Just arg){String}: text

|]

instance Show Text2 where
	show = text

instance Short Text2 where
	short t = show $ dotdot 10 10 $ text t

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

Curv2 deriving Show

arg :: Int

4: num_curv
(2, Just num_curv){[Int]}: body_curv

|]

instance Short Curv2 where
	short = dotdot 20 20 . show

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

MLUC2

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

instance Show MLUC2 where
	show mluc = "(MLUC2 " ++
		show (num_MLUC2 mluc) ++ " " ++
		dotdot 90 90 (show $ record_MLUC2 mluc) ++ " " ++
		dotdot 100 100 (show $ body_MLUC2 mluc) ++ ")"
--		show (map (body_MLUC2 mluc !!) [1, 3 .. 300]) ++ ")"

instance Short MLUC2 where
	short mluc = "(MLUC2 " ++
		show (num_MLUC2 mluc) ++ " " ++
		dotdot 20 20 (show $ record_MLUC2 mluc) ++ " " ++
		dotdot 40 40 (show $ body_MLUC2 mluc) ++ ")"

[binary|

MMOD2

arg :: Int

((), Just arg){String}: body_MMOD2

|]

instance Show MMOD2 where
	show mmod = "(MMOD2 " ++
		show (body_MMOD2 mmod) ++ ")"

instance Short MMOD2 where
	short mmod = "(MMOD2 " ++
		dotdot 10 10 (show $ body_MMOD2 mmod) ++ ")"

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

VCGT2

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

instance Show VCGT2 where
	show vcgt = "(VCGT2 " ++
		show (hoge_VCGT2 vcgt) ++ " " ++
		show (hage_VCGT2 vcgt) ++ " " ++
		show (hige_VCGT2 vcgt) ++ " " ++
		dotdot 100 100 (show $ body_VCGT2 vcgt) ++ ")"

[binary|

NDIN2

arg :: Int

(4, Just 9){[Int]}: hoge_NDIN2
(4, Just 3){[Int]}: hage_NDIN2
(2, Just 3){[Int]}: hige_NDIN2
(2, Just ((arg - 58) `div` 2)){[Int]}: body_NDIN2

|]

instance Show NDIN2 where
	show ndin = "(NDIN2 " ++
		show (hoge_NDIN2 ndin) ++ " " ++
		show (hage_NDIN2 ndin) ++ " " ++
		show (hige_NDIN2 ndin) ++ " " ++
		dotdot 100 100 (show $ body_NDIN2 ndin) ++ ")"
