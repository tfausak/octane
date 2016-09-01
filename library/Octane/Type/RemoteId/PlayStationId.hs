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

module Octane.Type.RemoteId.PlayStationId
  ( PlayStationId(..)
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf

data PlayStationId = PlayStationId
  { playStationIdName :: Text.Text
  , playStationIdUnknown :: LazyBytes.ByteString
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''PlayStationId)

-- | Each part is stored as exactly 16 bits.
instance BinaryBit.BinaryBit PlayStationId where
  getBits _ = do
    nameBytes <- BinaryBit.getByteString 16
    let name =
          nameBytes & Endian.reverseBitsInStrictBytes & Encoding.decodeLatin1 &
          StrictText.dropWhileEnd (== '\0') &
          Text.Text
    unknownBytes <- BinaryBit.getByteString 16
    let unknown =
          unknownBytes & Endian.reverseBitsInStrictBytes & LazyBytes.fromStrict
    pure (PlayStationId name unknown)
  putBits _ playStationId = do
    playStationId & #name & #unpack & StrictText.justifyLeft 16 '\x00' &
      StrictText.take 16 &
      Text.encodeLatin1 &
      Endian.reverseBitsInStrictBytes &
      BinaryBit.putByteString
    playStationId & #unknown & LazyBytes.toStrict &
      Endian.reverseBitsInStrictBytes &
      BinaryBit.putByteString

instance DeepSeq.NFData PlayStationId

instance Aeson.ToJSON PlayStationId where
  toJSON playStationId =
    Aeson.object
      [ "Name" .= #name playStationId
      , "Unknown" .=
        (playStationId & #unknown & LazyBytes.unpack &
         concatMap (Printf.printf "%02x") &
         ("0x" ++) &
         StrictText.pack)
      ]
