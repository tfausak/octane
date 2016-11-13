{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Property.IntProperty
  ( IntProperty(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data IntProperty = IntProperty
  { intPropertySize :: Word64.Word64
  , intPropertyContent :: Int32.Int32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''IntProperty)

instance Aeson.ToJSON IntProperty where
  toJSON int =
    Aeson.object
      [ "Type" .= ("Int" :: Text.Text)
      , "Size" .= #size int
      , "Value" .= #content int
      ]
