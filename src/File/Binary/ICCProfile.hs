{-# LANGUAGE QuasiQuotes, TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module File.Binary.ICCProfile (ICCP(..)) where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import File.Binary.Instances.LSB0 (BitsInt)
import Control.Arrow
import Data.Time
import Data.Function
import Data.List

import File.Binary.ICCProfile.TagTypes

sortTags :: [Tag] -> [([String], Int)]
sortTags ts = unifyTags [] undefined undefined $ sortBy compareOffset ts
	where
	compareOffset = on compare tag_data_offset

unifyTags :: [String] -> Int -> Int -> [Tag] -> [([String], Int)]
unifyTags [] _ _ (t : ts) =
	unifyTags [tag_signature t] (tag_data_offset t) (tag_element_size t) ts
unifyTags bs _ sz [] = [(reverse bs, sz)]
unifyTags bs ofst sz (t : ts)
	| tag_data_offset t == ofst && tag_element_size t == sz =
		unifyTags (tag_signature t : bs) ofst sz ts
	| ofst + sz + padd sz == tag_data_offset t = (reverse bs, sz) : unifyTags
		[tag_signature t] (tag_data_offset t) (tag_element_size t) ts
	| otherwise = error "bad"

padd :: Int -> Int
padd n	| n `mod` 4 == 0 = 0
	| otherwise = 4 - n `mod` 4

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
sortTags tags{[Body]}: bodys

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
