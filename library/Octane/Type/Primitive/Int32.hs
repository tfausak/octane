{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Int32 (Int32(..)) where

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
import qualified Data.Int as Int
import qualified Data.Scientific as Scientific
import qualified GHC.Generics as Generics
import qualified Octane.Utility as Utility


-- | A 32-bit little-endian integer.
newtype Int32 = Int32
    { unpackInt32 :: Int.Int32
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Int32 where
    get = do
        value <- Binary.getWord32le
        value & fromIntegral & Int32 & pure

    put int32 = int32
        & unpackInt32
        & fromIntegral
        & Binary.putWord32le

instance BinaryBit.BinaryBit Int32 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 4
        let value = Binary.runGet
                Binary.getWord32le
                (bytes & LazyBytes.fromStrict & Utility.reverseBitsInBytes)
        value & fromIntegral & Int32 & pure

    putBits _ int32 = int32
        & unpackInt32
        & fromIntegral
        & Binary.putWord32le
        & Binary.runPut
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance Aeson.FromJSON Int32 where
    parseJSON json = case json of
        Aeson.Number number -> case Scientific.toBoundedInteger number of
            Just integer -> pure (Int32 integer)
            _ -> Aeson.typeMismatch "Int32" json
        _ -> Aeson.typeMismatch "Int32" json

instance DeepSeq.NFData Int32 where

instance Aeson.ToJSON Int32 where
    toJSON int32 = int32
        & unpackInt32
        & Aeson.toJSON
