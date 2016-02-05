{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Word32LE (Word32LE(..)) where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Octane.Core

-- | A 32-bit little-endian integer.
newtype Word32LE = Word32LE
    { getWord32LE :: Word32
    } deriving (Eq, Generic, NFData, Show)

instance Binary Word32LE where
    get = do
        int <- Binary.getWord32le
        int & fromIntegral & Word32LE & return

    put (Word32LE int) = do
        int & fromIntegral & Binary.putWord32le
