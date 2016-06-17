{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Word8 (Word8(..), fromWord8) where

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
import qualified Data.Word as Word
import qualified Data.Scientific as Scientific
import qualified GHC.Generics as Generics
import qualified Octane.Utility as Utility


-- | An 8-bit little-endian wordeger.
newtype Word8 = Word8
    { unpackWord8 :: Word.Word8
    } deriving (Eq, Generics.Generic, Num, Ord, Show)

instance BinaryBit.BinaryBit Word8 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 1
        let value = Binary.runGet
                Binary.getWord8
                (bytes & LazyBytes.fromStrict & Utility.reverseBitsInBytes)
        value & Word8 & pure

    putBits _ word8 = word8
        & unpackWord8
        & Binary.putWord8
        & Binary.runPut
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance Aeson.FromJSON Word8 where
    parseJSON json = case json of
        Aeson.Number number -> case Scientific.toBoundedInteger number of
            Just integer -> pure (Word8 integer)
            _ -> Aeson.typeMismatch "Word8" json
        _ -> Aeson.typeMismatch "Word8" json

instance DeepSeq.NFData Word8 where

instance Aeson.ToJSON Word8 where
    toJSON word8 = word8
        & unpackWord8
        & Aeson.toJSON


fromWord8 :: (Integral a) => Word8 -> a
fromWord8 word8 = word8 & unpackWord8 & fromIntegral
