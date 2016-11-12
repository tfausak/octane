{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Property.FloatProperty
  ( FloatProperty(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data FloatProperty = FloatProperty
  { floatPropertySize :: Word64.Word64
  , floatPropertyContent :: Float32.Float32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''FloatProperty)

instance Aeson.ToJSON FloatProperty where
  toJSON float =
    Aeson.object
      [ "Type" .= ("Float" :: Text.Text)
      , "Size" .= #size float
      , "Value" .= #content float
      ]
