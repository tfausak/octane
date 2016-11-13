{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Property.ByteProperty
  ( ByteProperty(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data ByteProperty = ByteProperty
  { bytePropertySize :: Word64.Word64
  , bytePropertyKey :: Text.Text
  , bytePropertyValue :: Text.Text
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ByteProperty)

instance Aeson.ToJSON ByteProperty where
  toJSON byte =
    Aeson.object
      [ "Type" .= ("Byte" :: Text.Text)
      , "Size" .= #size byte
      , "Value" .= (#key byte, #value byte)
      ]
