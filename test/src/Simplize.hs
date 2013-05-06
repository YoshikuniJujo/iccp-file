{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Simplize where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import Data.List
import Data.Function
import Data.Maybe
import Data.Monoid
import Control.Applicative

[binary|

Simple deriving Show

4: num
replicate num (){[Tag]}: tags
sortTags tags{[Data]}: dats

|]

[binary|

Tag deriving Show

replicate 4 (){String}: signature
4: offset
4: size

|]

[binary|

Data deriving Show

arg :: ([String], Int)

fst arg{Signatures}: signatures
replicate (snd arg) (){String}: dat
padding (snd arg): 0

|]

data Signatures = Signatures [String] deriving Show

instance Field Signatures where
	type FieldArgument Signatures = [String]
	fromBinary sigs bs = return (Signatures sigs, bs)
	toBinary _ _ = return mempty

sortTags :: [Tag] -> [([String], Int)]
sortTags tags = unifyTags [] undefined undefined $ sortBy compareOffset tags
	where
	compareOffset t1 t2 = compare (offset t1) (offset t2)

unifyTags :: [String] -> Int -> Int -> [Tag] -> [([String], Int)]
unifyTags [] _ _ (t : ts) = unifyTags [signature t] (offset t) (size t) ts
unifyTags bs _ sz [] = [(reverse bs, sz)]
unifyTags bs ofst sz (t : ts)
	| offset t == ofst && size t == sz =
		unifyTags (signature t : bs) ofst sz ts
	| ofst + sz + padding sz == offset t =
		(reverse bs, sz) : unifyTags [signature t] (offset t) (size t) ts
	| otherwise = error "bad"

padding :: Int -> Int
padding n
	| n `mod` 4 == 0 = 0
	| otherwise = 4 - n `mod` 4

testDat :: Simple
testDat = Simple 2 [
	Tag "some" 28 8,
	Tag "hoge" 36 4
 ] [
	Data (Signatures ["some"]) "01234567",
	Data (Signatures ["hoge"]) "abcd"]

testDat2 :: Simple
testDat2 = Simple 3 [
	Tag "fst " 0 5,
	Tag "snd " 8 5,
	Tag "thrd" 0 5
 ] [
	Data (Signatures ["fst ", "thrd"]) "hello",
	Data (Signatures ["snd "]) "world"
 ]

data Ref a = Raw a | Ref Int deriving Show

tagToRef :: (a -> a -> Bool) -> [a] -> [Ref a]
tagToRef same = reverse . (($) <$> ttr same . subtract 2 . length <*> reverse)

ttr :: (a -> a -> Bool) -> Int -> [a] -> [Ref a]
ttr _ _ [] = []
ttr same ptr (x : xs) = maybe (Raw x) (Ref . (ptr -)) (findIndex (same x) xs)
	: ttr same (ptr - 1) xs

preprocess :: Simple -> Simple
preprocess s = Simple (num s) (tags s) $ sortSimple s

sortSimple :: Simple -> [Data]
sortSimple (Simple n ts ds) = checkTags ts' `seq` ds'
	where
	(ts', ds') = unzip $ nubTagData $ sortBy (on compareOffset fst) $ zip ts ds
	compareOffset (Tag _ o1 _) (Tag _ o2 _) = compare o1 o2

checkTags :: [Tag] -> ()
checkTags [_] = ()
checkTags (t1 : ts@(t2 : _)) =
	if offset t1 + size t1 + padding (size t1) == offset t2
		then checkTags ts
		else error "bad"

nubTagData :: [(Tag, Data)] -> [(Tag, Data)]
nubTagData [] = []
nubTagData ((t0, d0) : rest) = let
	sames = filter ((== offset t0) . offset . fst) rest
	check t0 d0 t1 d1 = size t0 == size t1 && dat d0 == dat d1
	gate = if all (uncurry $ check t0 d0) sames  then () else error "bad" in
	gate `seq` (t0, d0) :
		nubTagData (filter ((/= offset t0) . offset . fst) rest)
	

mkSimple :: [(String, Data)] -> Simple

mkSimple dats =
--	Simple (length dats) (reverse $ fst $ foldl mkTag ([], 0) dats) $ map snd dats
	Simple (length dats) (reverse $ foldl mkTag' [] dats) $ map snd dats

mkTag :: ([Tag], Int) -> (String, Data) -> ([Tag], Int)
mkTag (ts, offset) (sig, Data _ dat) = (nt : ts, offset + len + padding len)
	where
	len = length dat
	nt = Tag sig offset len

mkTag' :: [Tag] -> (String, Data) -> [Tag]
mkTag' [] (sig, Data _ dat) = [Tag sig 0 $ length dat]
mkTag' ts@(Tag _ ofst sz : _) (sig, Data _ dat) = nt : ts
	where
	len = length dat
	nt = Tag sig (ofst + sz + padding sz) len
