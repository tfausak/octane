{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Property.NameProperty
  ( NameProperty(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data NameProperty = NameProperty
  { namePropertySize :: Word64.Word64
  , namePropertyContent :: Text.Text
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''NameProperty)

instance Aeson.ToJSON NameProperty where
  toJSON name =
    Aeson.object
      [ "Type" .= ("Name" :: Text.Text)
      , "Size" .= #size name
      , "Value" .= #content name
      ]
