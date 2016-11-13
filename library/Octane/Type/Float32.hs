{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Float32
  ( Float32(..)
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords

-- | A 32-bit float.
newtype Float32 = Float32
  { float32Unpack :: Float
  } deriving (Eq, Fractional, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Float32)

-- | Shown as @12.34@.
instance Show Float32 where
  show float32 = show (#unpack float32)

-- | Encoded directly as a JSON number.
instance Aeson.ToJSON Float32 where
  toJSON float32 = float32 & #unpack & Aeson.toJSON
