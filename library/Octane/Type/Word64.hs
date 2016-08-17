module Octane.Type.Word64 (Word64(..), fromWord64, toWord64) where

import Basics

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Default.Class as Default
import qualified Data.Word as Word
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf


-- | A 64-bit unsigned integer.
newtype Word64 = Word64
    { word64Unpack :: Word.Word64
    } deriving (Eq, Generic, Num, Ord)

$(overloadedRecord Default.def ''Word64)

-- | Little-endian.
instance Binary.Binary Word64 where
    get = do
        value <- Binary.getWord64le
        pure (Word64 value)

    put word64 = do
        let value = #unpack word64
        Binary.putWord64le value

-- | Little-endian with the bits in each byte reversed.
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

instance NFData Word64 where

-- | Shown as @0x0102030405060708@.
instance Show Word64 where
    show word64 = Printf.printf "0x%016x" (#unpack word64)

-- | Encoded as a JSON number.
instance Aeson.ToJSON Word64 where
    toJSON word64 = word64
        & #unpack
        & Aeson.toJSON


-- | Converts a 'Word64' into any 'Integral' value.
fromWord64 :: (Integral a) => Word64 -> a
fromWord64 word64 = fromIntegral (#unpack word64)


-- | Converts any 'Integral' value into a 'Word64'.
toWord64 :: (Integral a) => a -> Word64
toWord64 value = Word64 (fromIntegral value)
