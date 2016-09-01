{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.RemoteId.SteamId
  ( SteamId(..)
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word64 as Word64

newtype SteamId = SteamId
  { steamIdUnpack :: Word64.Word64
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''SteamId)

-- | Stored as a plain 'Word64.Word64'.
instance BinaryBit.BinaryBit SteamId where
  getBits _ = do
    steamId <- BinaryBit.getBits 0
    pure (SteamId steamId)
  putBits _ steamId = steamId & #unpack & BinaryBit.putBits 0

instance DeepSeq.NFData SteamId

instance Aeson.ToJSON SteamId where
  toJSON steamId =
    Aeson.object
      ["Type" .= ("Steam" :: StrictText.Text), "Value" .= #unpack steamId]
