{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Float32 (Float32(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.IEEE754 as IEEE754
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified GHC.Generics as Generics
import qualified Octane.Utility.Endian as Endian


-- | A 32-bit float.
newtype Float32 = Float32
    { unpack :: Float
    } deriving (Eq, Fractional, Generics.Generic, Num, Ord)

-- | Stored little-endian.
instance Binary.Binary Float32 where
    get = do
        value <- IEEE754.getFloat32le
        value & Float32 & pure

    put float32 = float32
        & unpack
        & IEEE754.putFloat32le

-- | Stored little-endian with the bits in each byte reversed.
instance BinaryBit.BinaryBit Float32 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 4
        bytes
            & LazyBytes.fromStrict
            & Endian.reverseBitsInBytes
            & Binary.runGet Binary.get
            & pure

    putBits _ float32 = float32
        & Binary.put
        & Binary.runPut
        & Endian.reverseBitsInBytes
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance DeepSeq.NFData Float32

-- | Shown as @12.34@.
instance Show Float32 where
    show float32 = show (unpack float32)

-- | Encoded directly as a JSON number.
instance Aeson.ToJSON Float32 where
    toJSON float32 = float32
        & unpack
        & Aeson.toJSON
