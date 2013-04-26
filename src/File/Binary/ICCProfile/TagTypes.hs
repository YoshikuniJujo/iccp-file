{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module File.Binary.ICCProfile.TagTypes where

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()

[binary|

Curv deriving Show

arg :: Int

4: num_curv
(2, Just num_curv){[Int]}: body_curv

|]
