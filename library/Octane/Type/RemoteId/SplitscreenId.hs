{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.RemoteId.SplitscreenId
  ( SplitscreenId(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics

newtype SplitscreenId = SplitscreenId
  { splitscreenIdUnpack :: Maybe Int
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''SplitscreenId)

-- | Stored as a bare byte string.
instance BinaryBit.BinaryBit SplitscreenId where
  getBits _ = do
    bytes <- BinaryBit.getByteString 3
    case bytes of
      "\x00\x00\x00" -> do
        pure (SplitscreenId (Just 0))
      _ -> do
        fail ("Unexpected SplitscreenId value " ++ show bytes)
  putBits _ _splitscreenId = do
    BinaryBit.putByteString "\x00\x00\x00"

instance DeepSeq.NFData SplitscreenId

instance Aeson.ToJSON SplitscreenId where
  toJSON splitscreenId =
    Aeson.object
      [ "Type" .= ("Splitscreen" :: StrictText.Text)
      , "Value" .= #unpack splitscreenId
      ]
