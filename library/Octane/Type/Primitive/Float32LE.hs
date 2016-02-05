{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Float32LE (Float32LE(..)) where

import qualified Data.Binary.IEEE754 as IEEE754
import Octane.Core

-- | A 32-bit little-endian float.
newtype Float32LE = Float32LE
    { getFloat32LE :: Float
    } deriving (Eq, Generic, NFData, Show)

instance Binary Float32LE where
    get = do
        float <- IEEE754.getFloat32le
        float & Float32LE & return

    put (Float32LE float) = do
        float & IEEE754.putFloat32le
