{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Word64LE (Word64LE(..)) where

import Octane.Internal.Core

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary

-- | A 64-bit little-endian integer.
newtype Word64LE = Word64LE Word64
    deriving (Eq, Generic, NFData, Show)

instance Binary Word64LE where
    get = do
        word <- Binary.getWord64le
        word & fromIntegral & pack & return

    put word = do
        word & unpack & fromIntegral & Binary.putWord64le

instance Newtype Word64LE

instance ToJSON Word64LE
