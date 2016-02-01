{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Float32LE (Float32LE(..)) where

import Octane.Core

newtype Float32LE = Float32LE
    { getFloat32LE :: Float
    } deriving (Eq, Generic, NFData, Show)

instance Binary Float32LE where
    get = do
        float <- getFloat32le
        float & Float32LE & return

    put (Float32LE float) = do
        float & putFloat32le
