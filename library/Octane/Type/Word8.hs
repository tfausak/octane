module Octane.Type.Word8 (Word8(..), fromWord8, toWord8) where

import Basics

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Word as Word
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf


-- | A 8-bit unsigned integer.
newtype Word8 = Word8
    { word8Unpack :: Word.Word8
    } deriving (Eq, Generic, Num, Ord)

$(overloadedRecord def ''Word8)

instance Binary Word8 where
    get = do
        value <- Binary.getWord8
        pure (Word8 value)

    put word8 = do
        let value = #unpack word8
        Binary.putWord8 value

-- | The bits are reversed.
instance BinaryBit Word8 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 1
        bytes
            & LazyBytes.fromStrict
            & Endian.reverseBitsInLazyBytes
            & Binary.runGet Binary.get
            & pure

    putBits _ word8 = word8
        & Binary.put
        & Binary.runPut
        & Endian.reverseBitsInLazyBytes
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance NFData Word8 where

-- | Shown as @0x01@.
instance Show Word8 where
    show word8 = Printf.printf "0x%02x" (#unpack word8)

-- | Encoded as a JSON number.
instance ToJSON Word8 where
    toJSON word8 = word8
        & #unpack
        & toJSON


-- | Converts a 'Word8' into any 'Integral' value.
fromWord8 :: (Integral a) => Word8 -> a
fromWord8 word8 = fromIntegral (#unpack word8)


-- | Converts any 'Integral' value into a 'Word8'.
toWord8 :: (Integral a) => a -> Word8
toWord8 value = Word8 (fromIntegral value)
