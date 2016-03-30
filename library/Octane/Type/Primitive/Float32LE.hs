{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Float32LE (Float32LE(..)) where

import qualified Data.Binary.IEEE754 as IEEE754
import Octane.Internal.Core

-- | A 32-bit little-endian float.
newtype Float32LE = Float32LE Float
    deriving (Eq, Generic, NFData, Show)

instance Binary Float32LE where
    get = do
        float <- IEEE754.getFloat32le
        float & pack & return

    put float = do
        float & unpack & IEEE754.putFloat32le

instance Newtype Float32LE

instance ToJSON Float32LE
