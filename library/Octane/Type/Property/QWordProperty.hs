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

module Octane.Type.Property.QWordProperty
  ( QWordProperty(..)
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

data QWordProperty = QWordProperty
  { qWordPropertySize :: Word64.Word64
  , qWordPropertyContent :: Word64.Word64
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''QWordProperty)

instance Binary.Binary QWordProperty where
  get = do
    size <- Binary.get
    content <-
      case #unpack size of
        8 -> Binary.get
        x -> fail ("unknown QWordProperty size " ++ show x)
    pure (QWordProperty size content)
  put qWord = do
    qWord & #size & Binary.put
    qWord & #content & Binary.put

instance DeepSeq.NFData QWordProperty

instance Aeson.ToJSON QWordProperty where
  toJSON qWord =
    Aeson.object
      [ "Type" .= ("QWord" :: Text.Text)
      , "Size" .= #size qWord
      , "Value" .= #content qWord
      ]
