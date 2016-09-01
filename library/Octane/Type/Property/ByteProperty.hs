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

module Octane.Type.Property.ByteProperty
  ( ByteProperty(..)
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

data ByteProperty = ByteProperty
  { bytePropertySize :: Word64.Word64
  , bytePropertyKey :: Text.Text
  , bytePropertyValue :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ByteProperty)

instance Binary.Binary ByteProperty where
  get = do
    size <- Binary.get
    key <- Binary.get
    if key == "OnlinePlatform_Steam"
      then do
        pure (ByteProperty size "OnlinePlatform" key)
      else do
        value <- Binary.get
        pure (ByteProperty size key value)
  put byte = do
    byte & #size & Binary.put
    byte & #key & Binary.put
    byte & #value & Binary.put

instance DeepSeq.NFData ByteProperty

instance Aeson.ToJSON ByteProperty where
  toJSON byte =
    Aeson.object
      [ "Type" .= ("Byte" :: Text.Text)
      , "Size" .= #size byte
      , "Value" .= (#key byte, #value byte)
      ]
