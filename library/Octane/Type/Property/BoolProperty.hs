{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Property.BoolProperty
  ( BoolProperty(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data BoolProperty = BoolProperty
  { boolPropertySize :: Word64.Word64
  , boolPropertyContent :: Boolean.Boolean
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''BoolProperty)

instance Aeson.ToJSON BoolProperty where
  toJSON bool =
    Aeson.object
      [ "Type" .= ("Bool" :: Text.Text)
      , "Size" .= #size bool
      , "Value" .= #content bool
      ]
