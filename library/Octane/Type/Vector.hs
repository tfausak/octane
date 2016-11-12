{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Vector
  ( Vector(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords

-- | Three values packed together. Although the fields are called @x@, @y@, and
-- @z@, that may not be what they actually represent.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it is not
-- always serialized the same way. Sometimes it is three values run together,
-- but other times it has a flag for the presence of each value.
data Vector a = Vector
  { vectorX :: a
  , vectorY :: a
  , vectorZ :: a
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Vector)

-- | Encoded as a JSON array with 3 elements.
--
-- Aeson.encode (Vector 1 2 3 :: Vector Int)
-- "[1,2,3]"
instance (Aeson.ToJSON a) =>
         Aeson.ToJSON (Vector a) where
  toJSON vector = Aeson.toJSON [#x vector, #y vector, #z vector]
