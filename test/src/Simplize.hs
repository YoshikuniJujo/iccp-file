{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Simplize where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()

[binary|

Simple deriving Show

4: num
((), Just num){[Tag]}: tags

|]

[binary|

Tag deriving Show

((), Just 4){String}: signature
4: offset
4: size

|]

[binary|

Data deriving Show

arg :: Int

((), Just arg){String}: dat
padding arg: 0

|]

padding :: Int -> Int
padding n
	| n `mod` 4 == 0 = 0
	| otherwise = 4 - n `mod` 4
