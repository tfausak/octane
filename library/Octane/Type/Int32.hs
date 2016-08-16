module Octane.Type.Int32 (Int32(..), fromInt32, toInt32) where

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
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Utility.Endian as Endian


-- | A 32-bit signed integer.
newtype Int32 = Int32
    { int32Unpack :: Int.Int32
    } deriving (Eq, Generic, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Int32)

-- | Little-endian.
instance Binary.Binary Int32 where
    get = do
        value <- Binary.getInt32le
        pure (Int32 value)

    put int32 = do
        let value = #unpack int32
        Binary.putInt32le value

-- | Little-endian with the bits in each byte reversed.
instance BinaryBit.BinaryBit Int32 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 4
        bytes
            & LazyBytes.fromStrict
            & Endian.reverseBitsInLazyBytes
            & Binary.runGet Binary.get
            & pure

    putBits _ int32 = int32
        & Binary.put
        & Binary.runPut
        & Endian.reverseBitsInLazyBytes
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance NFData Int32 where

-- | Shown as @1234@.
instance Show Int32 where
    show int32 = show (#unpack int32)

-- | Encoded as a JSON number directly.
instance Aeson.ToJSON Int32 where
    toJSON int32 = int32
        & #unpack
        & Aeson.toJSON


-- | Converts a 'Int32' into any 'Integral' value.
fromInt32 :: (Integral a) => Int32 -> a
fromInt32 int32 = fromIntegral (#unpack int32)


-- | Converts any 'Integral' value into a 'Int32'.
toInt32 :: (Integral a) => a -> Int32
toInt32 value = Int32 (fromIntegral value)
