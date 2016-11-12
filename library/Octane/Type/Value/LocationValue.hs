{-# LANGUAGE DataKinds #-}
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

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Vector as Vector

newtype LocationValue = LocationValue
  { locationValueUnpack :: Vector.Vector Int
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''LocationValue)

instance Aeson.ToJSON LocationValue where
  toJSON x =
    Aeson.object
      ["Type" .= ("Position" :: StrictText.Text), "Value" .= #unpack x]
