{- |
    A little-endian 32-bit float.
-}
module Octane.Parser.Types.Float32LE where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.IEEE754 as Binary
import Flow ((|>))

newtype Float32LE = NewFloat32LE {
    getFloat32LE :: Float
} deriving (Show)

instance Aeson.ToJSON Float32LE where
    toJSON (NewFloat32LE float32LE) = Aeson.toJSON float32LE

instance Binary.Binary Float32LE where
    get = do
        float32LE <- Binary.getFloat32le
        return NewFloat32LE {
            getFloat32LE = float32LE
        }

    put (NewFloat32LE float32LE) = do
        float32LE |> Binary.putFloat32le
