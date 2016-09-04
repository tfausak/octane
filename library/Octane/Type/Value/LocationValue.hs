{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.LocationValue
  ( LocationValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Vector as Vector

newtype LocationValue = LocationValue
  { locationValueUnpack :: Vector.Vector Int
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData LocationValue

instance Aeson.ToJSON LocationValue where
  toJSON x =
    Aeson.object
      ["Type" .= ("Position" :: StrictText.Text), "Value" .= #unpack x]

$(OverloadedRecords.overloadedRecord Default.def ''LocationValue)
