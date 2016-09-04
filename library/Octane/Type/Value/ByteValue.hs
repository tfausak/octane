{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.ByteValue
  ( ByteValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word8 as Word8

newtype ByteValue = ByteValue
  { byteValueUnpack :: Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ByteValue

instance Aeson.ToJSON ByteValue where
  toJSON x =
    Aeson.object ["Type" .= ("Byte" :: StrictText.Text), "Value" .= #unpack x]

$(OverloadedRecords.overloadedRecord Default.def ''ByteValue)
