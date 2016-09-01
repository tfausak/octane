{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data NameProperty = NameProperty
  { namePropertySize :: Word64.Word64
  , namePropertyContent :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''NameProperty)

instance Binary.Binary NameProperty where
  get = do
    size <- Binary.get
    content <- Binary.get
    pure (NameProperty size content)
  put name = do
    name & #size & Binary.put
    name & #content & Binary.put

instance DeepSeq.NFData NameProperty

instance Aeson.ToJSON NameProperty where
  toJSON name =
    Aeson.object
      [ "Type" .= ("Name" :: Text.Text)
      , "Size" .= #size name
      , "Value" .= #content name
      ]
