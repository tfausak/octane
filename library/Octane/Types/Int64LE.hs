module Octane.Types.Int64LE where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Int as Int
import Octane.Core

newtype Int64LE = NewInt64LE {
    getInt64LE :: Int.Int64
} deriving (Show)

instance Binary.Binary Int64LE where
    get = do
        int <- Binary.getWord64le
        int & fromIntegral & NewInt64LE & return

    put (NewInt64LE int) = do
        int & fromIntegral & Binary.putWord64le
