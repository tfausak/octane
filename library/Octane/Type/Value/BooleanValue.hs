{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.BooleanValue
  ( BooleanValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean

newtype BooleanValue = BooleanValue
  { booleanValueUnpack :: Boolean.Boolean
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''BooleanValue)

instance Aeson.ToJSON BooleanValue where
  toJSON x =
    Aeson.object
      ["Type" .= ("Boolean" :: StrictText.Text), "Value" .= #unpack x]
