module Octane.Types.Float32LE where

import qualified Data.Binary as Binary
import qualified Data.Binary.IEEE754 as Binary
import Data.Function ((&))

newtype Float32LE = NewFloat32LE {
    getFloat32LE :: Float
} deriving (Show)

instance Binary.Binary Float32LE where
    get = do
        float <- Binary.getFloat32le
        float & NewFloat32LE & return

    put (NewFloat32LE float) = do
        float & Binary.putFloat32le
