{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Int32
  ( Int32(..)
  , fromInt32
  , toInt32
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.Int as Int
import qualified Data.OverloadedRecords.TH as OverloadedRecords

-- | A 32-bit signed integer.
newtype Int32 = Int32
  { int32Unpack :: Int.Int32
  } deriving (Enum, Eq, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Int32)

-- | Shown as @1234@.
instance Show Int32 where
  show int32 = show (#unpack int32)

-- | Encoded as a JSON number directly.
instance Aeson.ToJSON Int32 where
  toJSON int32 = int32 & #unpack & Aeson.toJSON

-- | Converts a 'Int32' into any 'Integral' value.
fromInt32
  :: (Integral a)
  => Int32 -> a
fromInt32 int32 = fromIntegral (#unpack int32)

-- | Converts any 'Integral' value into a 'Int32'.
toInt32
  :: (Integral a)
  => a -> Int32
toInt32 value = Int32 (fromIntegral value)
