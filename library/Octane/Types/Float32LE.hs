{- |
    A little-endian 32-bit float.
-}
module Octane.Types.Float32LE where

import qualified Data.Binary as Binary
import qualified Data.Binary.IEEE754 as Binary
import Flow ((|>))

newtype Float32LE = NewFloat32LE {
    getFloat32LE :: Float
} deriving (Show)

instance Binary.Binary Float32LE where
    get = do
        float32LE <- Binary.getFloat32le
        return NewFloat32LE {
            getFloat32LE = float32LE
        }

    put (NewFloat32LE float32LE) = do
        float32LE |> Binary.putFloat32le
