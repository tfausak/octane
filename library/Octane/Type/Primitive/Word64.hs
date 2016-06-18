{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Word64 (Word64(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Scientific as Scientific
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Octane.Utility as Utility


-- | A 64-bit little-endian unsigned integer.
newtype Word64 = Word64
    { unpackWord64 :: Word.Word64
    } deriving (Eq, Generics.Generic, Num, Show)

instance Binary.Binary Word64 where
    get = do
        value <- Binary.getWord64le
        value & Word64 & return

    put word64 = word64
        & unpackWord64
        & Binary.putWord64le

instance BinaryBit.BinaryBit Word64 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 8
        let value = Binary.runGet
                Binary.getWord64le
                (bytes & LazyBytes.fromStrict & Utility.reverseBitsInBytes)
        value & fromIntegral & Word64 & pure

    putBits _ word64 = word64
        & unpackWord64
        & fromIntegral
        & Binary.putWord64le
        & Binary.runPut
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance Aeson.FromJSON Word64 where
    parseJSON json = case json of
        Aeson.Number number -> case Scientific.toBoundedInteger number of
            Just integer -> pure (Word64 integer)
            _ -> Aeson.typeMismatch "Word64" json
        _ -> Aeson.typeMismatch "Word64" json

instance DeepSeq.NFData Word64

instance Aeson.ToJSON Word64 where
    toJSON word64 = word64
        & unpackWord64
        & Aeson.toJSON
