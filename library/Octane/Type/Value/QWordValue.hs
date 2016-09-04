{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.QWordValue
  ( QWordValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word64 as Word64

newtype QWordValue = QWordValue
  { qWordValueUnpack :: Word64.Word64
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData QWordValue

instance Aeson.ToJSON QWordValue where
  toJSON x =
    Aeson.object ["Type" .= ("QWord" :: StrictText.Text), "Value" .= #unpack x]

$(OverloadedRecords.overloadedRecord Default.def ''QWordValue)
