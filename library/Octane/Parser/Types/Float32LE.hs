{- |
    A little-endian 32-bit float.
-}
module Octane.Parser.Types.Float32LE where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.IEEE754 as Binary
import Data.Function ((&))

newtype Float32LE = NewFloat32LE {
    getFloat32LE :: Float
} deriving (Show)

instance Aeson.ToJSON Float32LE where
    toJSON (NewFloat32LE float32LE) = Aeson.toJSON float32LE

instance Binary.Binary Float32LE where
    get = do
        float <- Binary.getFloat32le
        float & NewFloat32LE & return

    put (NewFloat32LE float) = do
        float & Binary.putFloat32le
