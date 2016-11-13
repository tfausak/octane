{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Word32
  ( Word32(..)
  , fromWord32
  , toWord32
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Word as Word
import qualified Text.Printf as Printf

-- | A 32-bit unsigned integer.
newtype Word32 = Word32
  { word32Unpack :: Word.Word32
  } deriving (Enum, Eq, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Word32)

-- | Shown as @0x01020304@.
instance Show Word32 where
  show word32 = Printf.printf "0x%08x" (#unpack word32)

-- | Encoded as a JSON number.
instance Aeson.ToJSON Word32 where
  toJSON word32 = word32 & #unpack & Aeson.toJSON

-- | Converts a 'Word32' into any 'Integral' value.
fromWord32
  :: (Integral a)
  => Word32 -> a
fromWord32 word32 = fromIntegral (#unpack word32)

-- | Converts any 'Integral' value into a 'Word32'.
toWord32
  :: (Integral a)
  => a -> Word32
toWord32 value = Word32 (fromIntegral value)
