{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.RemoteId.XboxId
  ( XboxId(..)
  ) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word64 as Word64

newtype XboxId = XboxId
  { xboxIdUnpack :: Word64.Word64
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''XboxId)

-- | Stored as a plain 'Word64.Word64'.
instance BinaryBit.BinaryBit XboxId where
  getBits _ = do
    xboxId <- BinaryBit.getBits 0
    pure (XboxId xboxId)
  putBits _ xboxId = xboxId & #unpack & BinaryBit.putBits 0

instance DeepSeq.NFData XboxId

-- | Encoded directly as a number.
instance Aeson.ToJSON XboxId where
  toJSON xboxId = xboxId & #unpack & Aeson.toJSON
