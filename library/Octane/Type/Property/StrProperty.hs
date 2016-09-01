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

module Octane.Type.Property.StrProperty
  ( StrProperty(..)
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

data StrProperty = StrProperty
  { strPropertySize :: Word64.Word64
  , strPropertyContent :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''StrProperty)

instance Binary.Binary StrProperty where
  get = do
    size <- Binary.get
    content <- Binary.get
    pure (StrProperty size content)
  put str = do
    str & #size & Binary.put
    str & #content & Binary.put

instance DeepSeq.NFData StrProperty

instance Aeson.ToJSON StrProperty where
  toJSON x =
    Aeson.object
      ["Type" .= ("Str" :: Text.Text), "Size" .= #size x, "Value" .= #content x]
