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

module Octane.Type.Property.ArrayProperty
  ( ArrayProperty(..)
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.List as List
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data ArrayProperty a = ArrayProperty
  { arrayPropertySize :: Word64.Word64
  , arrayPropertyContent :: List.List (Dictionary.Dictionary a)
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ArrayProperty)

instance (Binary.Binary a) =>
         Binary.Binary (ArrayProperty a) where
  get = do
    size <- Binary.get
    content <- Binary.get
    pure (ArrayProperty size content)
  put array = do
    array & #size & Binary.put
    array & #content & Binary.put

instance (DeepSeq.NFData a) =>
         DeepSeq.NFData (ArrayProperty a)

instance (Aeson.ToJSON a) =>
         Aeson.ToJSON (ArrayProperty a) where
  toJSON array =
    Aeson.object
      [ "Type" .= ("Array" :: Text.Text)
      , "Size" .= #size array
      , "Value" .= #content array
      ]
