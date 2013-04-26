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

[binary|

Data deriving Show

arg :: Int

4: type_data
((), Just (arg - 4)){String}: body_data

|]

[binary|

MFT2 deriving Show

arg :: Int

1: input_num_mft2
1: output_num_mft2
1: clut_num_mft2
1: 0
4: e1_mft2
4: e2_mft2
4: e3_mft2
4: e4_mft2
4: e5_mft2
4: e6_mft2
4: e7_mft2
4: e8_mft2
4: e9_mft2
2: input_table_n_mft2
2: output_table_n_mft2
(2, Just $ input_table_n_mft2 * input_num_mft2){[Int]}: input_table_mft2
(2, Just $ clut_num_mft2 ^ input_num_mft2 * output_num_mft2){[Int]}: clut_table_mft2
(2, Just $ output_table_n_mft2 * output_num_mft2){[Int]}: output_table_mft2

|]
