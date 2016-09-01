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

module Octane.Type.Property.BoolProperty
  ( BoolProperty(..)
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data BoolProperty = BoolProperty
  { boolPropertySize :: Word64.Word64
  , boolPropertyContent :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''BoolProperty)

instance Binary.Binary BoolProperty where
  get = do
    size <- Binary.get
    content <- Binary.get
    pure (BoolProperty size content)
  put bool = do
    bool & #size & Binary.put
    bool & #content & Binary.put

instance DeepSeq.NFData BoolProperty

instance Aeson.ToJSON BoolProperty where
  toJSON bool =
    Aeson.object
      [ "Type" .= ("Bool" :: Text.Text)
      , "Size" .= #size bool
      , "Value" .= #content bool
      ]
