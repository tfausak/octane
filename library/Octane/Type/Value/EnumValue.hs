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

module Octane.Type.Value.EnumValue
  ( EnumValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Word16 as Word16

data EnumValue = EnumValue
  { enumValueValue :: Word16.Word16
  , enumValueFlag :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData EnumValue

instance Aeson.ToJSON EnumValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Enum" :: StrictText.Text)
      , "Value" .= Aeson.object ["Value" .= #value x, "Flag" .= #flag x]
      ]

$(OverloadedRecords.overloadedRecord Default.def ''EnumValue)
