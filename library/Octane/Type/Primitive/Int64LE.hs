{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Int64LE (Int64LE(..)) where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Octane.Core

-- | A 64-bit little-endian integer.
newtype Int64LE = Int64LE
    { getInt64LE :: Int64
    } deriving (Eq, Generic, NFData, Show)

instance Binary Int64LE where
    get = do
        int <- Binary.getWord64le
        int & fromIntegral & Int64LE & return

    put (Int64LE int) = do
        int & fromIntegral & Binary.putWord64le
