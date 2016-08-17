module Octane.Type.Int8 (Int8(..), fromInt8, toInt8) where

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
import qualified Data.Int as Int
import qualified Octane.Utility.Endian as Endian


-- | A 8-bit signed integer.
newtype Int8 = Int8
    { int8Unpack :: Int.Int8
    } deriving (Eq, Generic, Num, Ord)

$(overloadedRecord Default.def ''Int8)

instance Binary.Binary Int8 where
    get = do
        value <- Binary.getInt8
        pure (Int8 value)

    put int8 = do
        let value = #unpack int8
        Binary.putInt8 value

-- | Stored with the bits reversed.
instance BinaryBit.BinaryBit Int8 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 1
        bytes
            & LazyBytes.fromStrict
            & Endian.reverseBitsInLazyBytes
            & Binary.runGet Binary.get
            & pure

    putBits _ int8 = int8
        & Binary.put
        & Binary.runPut
        & Endian.reverseBitsInLazyBytes
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance NFData Int8 where

-- | Shown as @1234@.
instance Show Int8 where
    show int8 = show (#unpack int8)

-- | Encoded directly as a JSON number.
instance Aeson.ToJSON Int8 where
    toJSON int8 = int8
        & #unpack
        & Aeson.toJSON


-- | Converts a 'Int8' into any 'Integral' value.
fromInt8 :: (Integral a) => Int8 -> a
fromInt8 int8 = fromIntegral (#unpack int8)


-- | Converts any 'Integral' value into a 'Int8'.
toInt8 :: (Integral a) => a -> Int8
toInt8 value = Int8 (fromIntegral value)
