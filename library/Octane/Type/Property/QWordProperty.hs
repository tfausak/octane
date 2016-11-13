{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Property.QWordProperty
  ( QWordProperty(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data QWordProperty = QWordProperty
  { qWordPropertySize :: Word64.Word64
  , qWordPropertyContent :: Word64.Word64
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''QWordProperty)

instance Aeson.ToJSON QWordProperty where
  toJSON qWord =
    Aeson.object
      [ "Type" .= ("QWord" :: Text.Text)
      , "Size" .= #size qWord
      , "Value" .= #content qWord
      ]
