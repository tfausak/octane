module Octane.Type.Float32 (Float32(..)) where

import Basics

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.IEEE754 as IEEE754
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Octane.Utility.Endian as Endian


-- | A 32-bit float.
newtype Float32 = Float32
    { float32Unpack :: Float
    } deriving (Eq, Fractional, Generic, Num, Ord)

$(overloadedRecord def ''Float32)

-- | Little-endian.
instance Binary.Binary Float32 where
    get = do
        value <- IEEE754.getFloat32le
        value & Float32 & pure

    put float32 = float32
        & #unpack
        & IEEE754.putFloat32le

-- | Little-endian with the bits in each byte reversed.
instance BinaryBit.BinaryBit Float32 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 4
        bytes
            & LazyBytes.fromStrict
            & Endian.reverseBitsInLazyBytes
            & Binary.runGet Binary.get
            & pure

    putBits _ float32 = float32
        & Binary.put
        & Binary.runPut
        & Endian.reverseBitsInLazyBytes
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance NFData Float32

-- | Shown as @12.34@.
instance Show Float32 where
    show float32 = show (#unpack float32)

-- | Encoded directly as a JSON number.
--
-- Aeson.encode (1.2 :: Float32)
-- "1.2"
instance Aeson.ToJSON Float32 where
    toJSON float32 = float32
        & #unpack
        & Aeson.toJSON
