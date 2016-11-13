{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Word64
  ( Word64(..)
  , fromWord64
  , toWord64
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Word as Word
import qualified Text.Printf as Printf

-- | A 64-bit unsigned integer.
newtype Word64 = Word64
  { word64Unpack :: Word.Word64
  } deriving (Eq, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Word64)

-- | Shown as @0x0102030405060708@.
instance Show Word64 where
  show word64 = Printf.printf "0x%016x" (#unpack word64)

-- | Encoded as a JSON number.
instance Aeson.ToJSON Word64 where
  toJSON word64 = word64 & #unpack & Aeson.toJSON

-- | Converts a 'Word64' into any 'Integral' value.
fromWord64
  :: (Integral a)
  => Word64 -> a
fromWord64 word64 = fromIntegral (#unpack word64)

-- | Converts any 'Integral' value into a 'Word64'.
toWord64
  :: (Integral a)
  => a -> Word64
toWord64 value = Word64 (fromIntegral value)
