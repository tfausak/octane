{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.StringValue
  ( StringValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Text as Text

newtype StringValue = StringValue
  { stringValueUnpack :: Text.Text
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''StringValue)

instance Aeson.ToJSON StringValue where
  toJSON x =
    Aeson.object ["Type" .= ("String" :: StrictText.Text), "Value" .= #unpack x]
