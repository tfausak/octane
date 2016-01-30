module Octane.Types.Int32LE (Int32LE(..)) where

import Octane.Core

newtype Int32LE = NewInt32LE
    { getInt32LE :: Int32
    } deriving (Show)

instance Binary Int32LE where
    get = do
        int <- getWord32le
        int & fromIntegral & NewInt32LE & return

    put (NewInt32LE int) = do
        int & fromIntegral & putWord32le
