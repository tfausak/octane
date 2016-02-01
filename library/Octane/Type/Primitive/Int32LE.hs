{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Int32LE (Int32LE(..)) where

import Octane.Core

-- | A 32-bit little-endian integer.
newtype Int32LE = Int32LE
    { getInt32LE :: Int32
    } deriving (Eq, Generic, NFData, Show)

instance Binary Int32LE where
    get = do
        int <- getWord32le
        int & fromIntegral & Int32LE & return

    put (Int32LE int) = do
        int & fromIntegral & putWord32le
