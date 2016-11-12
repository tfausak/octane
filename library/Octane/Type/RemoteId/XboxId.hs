{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.RemoteId.XboxId
  ( XboxId(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Word64 as Word64

newtype XboxId = XboxId
  { xboxIdUnpack :: Word64.Word64
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''XboxId)

instance Aeson.ToJSON XboxId where
  toJSON xboxId =
    Aeson.object
      ["Type" .= ("Xbox" :: StrictText.Text), "Value" .= #unpack xboxId]
