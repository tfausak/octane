{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.IntValue
  ( IntValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Int32 as Int32

newtype IntValue = IntValue
  { intValueUnpack :: Int32.Int32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''IntValue)

instance Aeson.ToJSON IntValue where
  toJSON x =
    Aeson.object ["Type" .= ("Int" :: StrictText.Text), "Value" .= #unpack x]
