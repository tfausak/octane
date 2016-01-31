module Octane.Type.Primitive.Float32LE (Float32LE(..)) where

import Octane.Core

newtype Float32LE = NewFloat32LE
    { getFloat32LE :: Float
    } deriving (Eq, Show)

instance Binary Float32LE where
    get = do
        float <- getFloat32le
        float & NewFloat32LE & return

    put (NewFloat32LE float) = do
        float & putFloat32le
