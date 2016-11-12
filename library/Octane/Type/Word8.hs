{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Word8
  ( Word8(..)
  , fromWord8
  , toWord8
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Word as Word
import qualified Text.Printf as Printf

-- | A 8-bit unsigned integer.
newtype Word8 = Word8
  { word8Unpack :: Word.Word8
  } deriving (Eq, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Word8)

-- | Shown as @0x01@.
instance Show Word8 where
  show word8 = Printf.printf "0x%02x" (#unpack word8)

-- | Encoded as a JSON number.
instance Aeson.ToJSON Word8 where
  toJSON word8 = word8 & #unpack & Aeson.toJSON

-- | Converts a 'Word8' into any 'Integral' value.
fromWord8
  :: (Integral a)
  => Word8 -> a
fromWord8 word8 = fromIntegral (#unpack word8)

-- | Converts any 'Integral' value into a 'Word8'.
toWord8
  :: (Integral a)
  => a -> Word8
toWord8 value = Word8 (fromIntegral value)
