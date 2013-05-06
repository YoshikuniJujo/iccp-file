{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.ICCProfile (
	ICCP(..), Tag(..), readICCP, writeICCP,
	paddings, sizes,
	duplicate, fromElems, filePadding, tagTypes,

	MLUC_Pre(..),
	MMOD2(..), Text2(..), Elem(..), Body(..)
) where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.Instances.LSB0 (BitsInt)
import Control.Arrow
import Control.Monad
import Data.Monoid
import Data.Time
import Control.Applicative

import File.Binary.ICCProfile.TagTypes

type ICCPData = (ICCP, [Element])

readICCP :: (Monad m, Applicative m, Binary b) => b -> m ICCPData
readICCP bin = do
	(ret, _) <- fromBinary () bin
	elems <- mapM (($ bin) . getElement') $ tags ret
	return (ret, elems)

tagTypes :: (Monad m, Applicative m, Binary b) => b -> m [String]
tagTypes bin = do
	(ret, _) <- fromBinary () bin
	tts <- mapM (($ bin) . getTagTypeName) $ tags ret
	return $ map tag_type_name tts

-- writeICCP :: (Monad m, Applicative m, Binary b) => ICCPData -> m b
-- writeICCP :: ICCPData -> IO String
writeICCP :: (Monad m, Applicative m) => ICCPData -> m String
writeICCP (ret, elems) = do
	let	dups = duplicate [] (tags ret)
--		pads = paddings (deleteIndexes dups $ tags ret) ++ [0]
		noDupTags = deleteIndexes dups (tags ret)
	bin <- toBinary () ret
--	bins <- mapM (toBinary (error "bad values") . snd) $ deleteIndexes dups elems
	bins <- zipWithM (\size dat -> toBinary size $ snd dat)
		(map tag_element_size noDupTags) $
			deleteIndexes dups elems
--	let bins' = zipWith (\d p -> d `mappend` replicate p '\0') bins pads
--	let	bins' = zipWith addPadding bins pads
	let	bins' = bins
	return $ mconcat (bin : bins') -- ++ replicate (filePadding ret) '\0'

filePadding :: ICCP -> Int
filePadding iccp = profile_size iccp -
	(tag_data_offset lastTag + tag_element_size lastTag)
	where
	lastTag = last $ tags iccp

fromElems :: (Monad m, Applicative m) => [Element] -> m [String]
fromElems = mapM $ toBinary undefined . snd

{-
addPadding :: String -> Int -> String
addPadding d p
	| p >= 0 = d `mappend` replicate p '\0'
	| otherwise = take (length d + p) d
-}

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
	| any (sameOffsetSize t) pre = length pre : duplicate (t : pre) post
	| otherwise = duplicate (t : pre) post

sameOffsetSize :: Tag -> Tag -> Bool
sameOffsetSize (Tag _ offset1 size1) (Tag _ offset2 size2) =
	offset1 == offset2 && size1 == size2

sizes :: [Tag] -> [Int]
sizes = map tag_element_size

getElement' :: (Monad m, Applicative m, Binary b) => Tag -> b -> m Element
getElement' t@(Tag tn _ _) str = do
	(d, _) <- getData t str
	return $ edata tn d

getTagTypeName :: (Monad m, Applicative m, Binary b) => Tag -> b -> m TagType
getTagTypeName (Tag _ offset _) = fmap fst . fromBinary () . snd . getBytes offset

[binary|

TagType deriving Show

replicate 4 (){String}: tag_type_name

|]

[binary|

ICCP deriving Show

4: profile_size
replicate 4 (){String}: cmm_type
1: profile_version_major
1: profile_version_minor
2: 0
replicate 4 (){String}: profile_device_class
replicate 4 (){String}: color_space_of_data
replicate 4 (){String}: profile_connection_space
{UTCTime}: create_time
4: "acsp"
replicate 4 (){String}: target_platform
2: profile_flags
{Bool}: is_embeded
{Bool}: only_embeded
14{BitsInt}: 0
4: device_manufacturer
4: device_model
4: device_attributes
{Bool}: device_trans
{Bool}: device_matte
{Bool}: device_nega
{Bool}: device_not_color
28{BitsInt}: 0
4: rendering_intent
{XYZ}: illuminant_value
replicate 4 (){String}: profile_creator
replicate 16 (){String}: profile_identifier
28{Integer}: 0
4: tag_count
replicate tag_count (){[Tag]}: tags

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

replicate 4 (){String}: tag_signature
4: tag_data_offset
4: tag_element_size

|]

edata :: String -> Body -> Element
edata = (,)

type Element = (String, Body)

getData :: (Monad m, Applicative m, Binary b) => Tag -> b -> m (Body, b)
getData (Tag _ offset size) = fromBinary size . snd . getBytes offset
