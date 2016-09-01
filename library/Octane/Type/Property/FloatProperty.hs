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

module Octane.Type.Property.FloatProperty
  ( FloatProperty(..)
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64

data FloatProperty = FloatProperty
  { floatPropertySize :: Word64.Word64
  , floatPropertyContent :: Float32.Float32
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''FloatProperty)

instance Binary.Binary FloatProperty where
  get = do
    size <- Binary.get
    content <-
      case #unpack size of
        4 -> Binary.get
        x -> fail ("unknown FloatProperty size " ++ show x)
    pure (FloatProperty size content)
  put float = do
    float & #size & Binary.put
    float & #content & Binary.put

instance DeepSeq.NFData FloatProperty

instance Aeson.ToJSON FloatProperty where
  toJSON float =
    Aeson.object
      [ "Type" .= ("Float" :: Text.Text)
      , "Size" .= #size float
      , "Value" .= #content float
      ]
