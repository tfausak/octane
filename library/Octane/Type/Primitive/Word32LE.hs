{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Word32LE (Word32LE(..)) where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Octane.Internal.Core

-- | A 32-bit little-endian integer.
newtype Word32LE = Word32LE Word32
    deriving (Eq, Generic, NFData, Show)

instance Binary Word32LE where
    get = do
        word <- Binary.getWord32le
        word & fromIntegral & pack & return

    put word = do
        word & unpack & fromIntegral & Binary.putWord32le

instance Newtype Word32LE

instance ToJSON Word32LE
