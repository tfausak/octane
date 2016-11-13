{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Property.StrProperty
  ( StrProperty(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data StrProperty = StrProperty
  { strPropertySize :: Word64.Word64
  , strPropertyContent :: Text.Text
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''StrProperty)

instance Aeson.ToJSON StrProperty where
  toJSON x =
    Aeson.object
      ["Type" .= ("Str" :: Text.Text), "Size" .= #size x, "Value" .= #content x]
