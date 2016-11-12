{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.FloatValue
  ( FloatValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Float32 as Float32

newtype FloatValue = FloatValue
  { floatValueUnpack :: Float32.Float32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''FloatValue)

instance Aeson.ToJSON FloatValue where
  toJSON x =
    Aeson.object ["Type" .= ("Float" :: StrictText.Text), "Value" .= #unpack x]
