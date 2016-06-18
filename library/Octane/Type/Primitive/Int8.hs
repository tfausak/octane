{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Int8 (Int8(..), fromInt8) where

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


-- | An 8-bit little-endian signed integer.
newtype Int8 = Int8
    { unpackInt8 :: Int.Int8
    } deriving (Eq, Generics.Generic, Num, Ord, Show)

instance BinaryBit.BinaryBit Int8 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 1
        let value = Binary.runGet
                Binary.getWord8
                (bytes & LazyBytes.fromStrict & Utility.reverseBitsInBytes)
        value & fromIntegral & Int8 & pure

    putBits _ int8 = int8
        & unpackInt8
        & fromIntegral
        & Binary.putWord8
        & Binary.runPut
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance Aeson.FromJSON Int8 where
    parseJSON json = case json of
        Aeson.Number number -> case Scientific.toBoundedInteger number of
            Just integer -> pure (Int8 integer)
            _ -> Aeson.typeMismatch "Int8" json
        _ -> Aeson.typeMismatch "Int8" json

instance DeepSeq.NFData Int8 where

instance Aeson.ToJSON Int8 where
    toJSON int8 = int8
        & unpackInt8
        & Aeson.toJSON


-- | A shortcut for unpacking an 'Int8' and converting it to some other
-- integral type.
fromInt8 :: (Integral a) => Int8 -> a
fromInt8 int8 = int8 & unpackInt8 & fromIntegral
