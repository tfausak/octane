{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octane.Type.Word64 (Word64(..), fromWord64, toWord64) where

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
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf

-- $setup
-- >>> import qualified Data.Binary.Get as Binary
-- >>> import qualified Data.Binary.Put as Binary


-- | A 64-bit unsigned integer.
newtype Word64 = Word64
    { word64Unpack :: Word.Word64
    } deriving (Eq, Generics.Generic, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Word64)

-- | Little-endian.
--
-- >>> Binary.decode "\x01\x00\x00\x00\x00\x00\x00\x00" :: Word64
-- 0x0000000000000001
--
-- >>> Binary.encode (1 :: Word64)
-- "\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
instance Binary.Binary Word64 where
    get = do
        value <- Binary.getWord64le
        pure (Word64 value)

    put word64 = do
        let value = #unpack word64
        Binary.putWord64le value

-- | Little-endian with the bits in each byte reversed.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x80\x00\x00\x00\x00\x00\x00\x00" :: Word64
-- 0x0000000000000001
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (1 :: Word64)))
-- "\128\NUL\NUL\NUL\NUL\NUL\NUL\NUL"
instance BinaryBit.BinaryBit Word64 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 8
        bytes
            & LazyBytes.fromStrict
            & Endian.reverseBitsInLazyBytes
            & Binary.runGet Binary.get
            & pure

    putBits _ word64 = word64
        & Binary.put
        & Binary.runPut
        & Endian.reverseBitsInLazyBytes
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance DeepSeq.NFData Word64 where

-- | Shown as @0x0102030405060708@.
--
-- >>> show (1 :: Word64)
-- "0x0000000000000001"
instance Show Word64 where
    show word64 = Printf.printf "0x%016x" (#unpack word64)

-- | Encoded as a JSON number.
--
-- >>> Aeson.encode (1 :: Word64)
-- "1"
instance Aeson.ToJSON Word64 where
    toJSON word64 = word64
        & #unpack
        & Aeson.toJSON


-- | Converts a 'Word64' into any 'Integral' value.
--
-- >>> fromWord64 0x0000000000000001 :: Word.Word64
-- 1
--
-- >>> fromWord64 0xffffffffffffffff :: Data.Int.Int64
-- -1
fromWord64 :: (Integral a) => Word64 -> a
fromWord64 word64 = fromIntegral (#unpack word64)


-- | Converts any 'Integral' value into a 'Word64'.
--
-- >>> toWord64 (1 :: Word.Word64)
-- 0x0000000000000001
--
-- >>> toWord64 (-1 :: Data.Int.Int64)
-- 0xffffffffffffffff
toWord64 :: (Integral a) => a -> Word64
toWord64 value = Word64 (fromIntegral value)
