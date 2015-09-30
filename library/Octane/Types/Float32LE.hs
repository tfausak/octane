module Octane.Types.Float32LE where

import qualified Data.Binary as B
import qualified Data.Binary.IEEE754 as B

newtype Float32LE = NewFloat32LE
    { unFloat32LE :: Float
    } deriving (Show)

instance B.Binary Float32LE where
    get = do
        float <- B.getFloat32le
        return (NewFloat32LE float)

    put (NewFloat32LE float) = do
        B.putFloat32le float
