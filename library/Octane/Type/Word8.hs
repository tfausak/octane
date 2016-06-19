{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Word8 (Word8(..), fromWord8, toWord8) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf


-- | A 8-bit unsigned integer.
newtype Word8 = Word8
    { unpack :: Word.Word8
    } deriving (Eq, Generics.Generic, Num, Ord)

instance Binary.Binary Word8 where
    get = do
        value <- Binary.getWord8
        pure (Word8 value)

    put word8 = do
        let value = unpack word8
        Binary.putWord8 value

instance BinaryBit.BinaryBit Word8 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 1
        bytes
            & LazyBytes.fromStrict
            & Endian.reverseBitsInBytes
            & Binary.runGet Binary.get
            & pure

    putBits _ word8 = word8
        & Binary.put
        & Binary.runPut
        & Endian.reverseBitsInBytes
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance DeepSeq.NFData Word8 where

-- | Shown as @0x01@.
instance Show Word8 where
    show word8 = Printf.printf "0x%02x" (unpack word8)

instance Aeson.ToJSON Word8 where
    toJSON word8 = word8
        & unpack
        & Aeson.toJSON


-- | Converts a 'Word8' into any 'Integral' value.
--
-- >>> fromWord8 0x01 :: Word.Word8
-- 1
--
-- >>> fromWord8 0xff :: Data.Int.Int8
-- -1
fromWord8 :: (Integral a) => Word8 -> a
fromWord8 word8 = fromIntegral (unpack word8)


-- | Converts any 'Integral' value into a 'Word8'.
--
-- >>> toWord8 (1 :: Word.Word8)
-- 0x01
--
-- >>> toWord8 (-1 :: Data.Int.Int8)
-- 0xff
toWord8 :: (Integral a) => a -> Word8
toWord8 value = Word8 (fromIntegral value)
