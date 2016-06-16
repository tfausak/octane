{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Float32 (Float32(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.IEEE754 as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Scientific as Scientific
import qualified GHC.Generics as Generics
import qualified Octane.Utility as Utility


-- | A 32-bit little-endian float.
newtype Float32 = Float32
    { unpackFloat32 :: Float
    } deriving (Eq, Fractional, Generics.Generic, Num, Ord, Show)

instance Binary.Binary Float32 where
    get = do
        value <- Binary.getFloat32le
        value & Float32 & pure

    put float32 = float32
        & unpackFloat32
        & Binary.putFloat32le

instance BinaryBit.BinaryBit Float32 where
    getBits _ = do
        bytes <- BinaryBit.getByteString 4
        let value = Binary.runGet
                Binary.getFloat32le
                (bytes & LazyBytes.fromStrict & Utility.reverseBitsInBytes)
        value & Float32 & pure

    putBits _ float32 = float32
        & unpackFloat32
        & Binary.putFloat32le
        & Binary.runPut
        & LazyBytes.toStrict
        & BinaryBit.putByteString

instance Aeson.FromJSON Float32 where
    parseJSON json = case json of
        Aeson.Number number -> case Scientific.toBoundedRealFloat number of
            Right float -> pure (Float32 float)
            _ -> Aeson.typeMismatch "Float32" json
        _ -> Aeson.typeMismatch "Float32" json

instance DeepSeq.NFData Float32

instance Aeson.ToJSON Float32 where
    toJSON float32 = float32
        & unpackFloat32
        & Aeson.toJSON
