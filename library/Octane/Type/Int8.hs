{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Int8
  ( Int8(..)
  , fromInt8
  , toInt8
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.Int as Int
import qualified Data.OverloadedRecords.TH as OverloadedRecords

-- | A 8-bit signed integer.
newtype Int8 = Int8
  { int8Unpack :: Int.Int8
  } deriving (Eq, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Int8)

-- | Shown as @1234@.
instance Show Int8 where
  show int8 = show (#unpack int8)

-- | Encoded directly as a JSON number.
instance Aeson.ToJSON Int8 where
  toJSON int8 = int8 & #unpack & Aeson.toJSON

-- | Converts a 'Int8' into any 'Integral' value.
fromInt8
  :: (Integral a)
  => Int8 -> a
fromInt8 int8 = fromIntegral (#unpack int8)

-- | Converts any 'Integral' value into a 'Int8'.
toInt8
  :: (Integral a)
  => a -> Int8
toInt8 value = Int8 (fromIntegral value)
