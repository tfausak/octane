module Octane.Types.Int64LE (Int64LE(..)) where

import Octane.Core

newtype Int64LE = NewInt64LE
    { getInt64LE :: Int64
    } deriving (Eq, Show)

instance Binary Int64LE where
    get = do
        int <- getWord64le
        int & fromIntegral & NewInt64LE & return

    put (NewInt64LE int) = do
        int & fromIntegral & putWord64le
