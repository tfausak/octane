{- |
    A little-endian 32-bit integer.
-}
module Octane.Types.Int32LE where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Int as Int
import Flow ((|>))

newtype Int32LE = NewInt32LE {
    getInt32LE :: Int.Int32
} deriving (Show)

instance Binary.Binary Int32LE where
    get = do
        word32LE <- Binary.getWord32le
        return NewInt32LE {
            getInt32LE = fromIntegral word32LE
        }

    put (NewInt32LE int32LE) = do
        int32LE |> fromIntegral |> Binary.putWord32le
