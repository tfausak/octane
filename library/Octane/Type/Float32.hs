{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Float32 (Float32(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.IEEE754 as IEEE754
import qualified Data.Scientific as Scientific
import qualified GHC.Generics as Generics


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

instance BinaryBit.BinaryBit Float32 where
    getBits _ = undefined

    putBits _ _ = undefined

instance Aeson.FromJSON Float32 where
    parseJSON json = case json of
        Aeson.Number number -> case Scientific.toBoundedRealFloat number of
            Left _ -> Aeson.typeMismatch "Float32" json
            Right float -> pure (Float32 float)
        _ -> Aeson.typeMismatch "Float32" json

instance DeepSeq.NFData Float32

instance Show Float32 where
    show float32 = show (unpack float32)

instance Aeson.ToJSON Float32 where
    toJSON float32 = float32
        & unpack
        & Aeson.toJSON
