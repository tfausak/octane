{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Word64LE (Word64LE(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.Function ((&))
import qualified Data.Word as Word
import qualified GHC.Generics as Generics

-- | A 64-bit little-endian integer.
newtype Word64LE =
    Word64LE Word.Word64
    deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Word64LE where
    get = do
        word <- Binary.getWord64le
        word & fromIntegral & Newtype.pack & return
    put word = do
        word & Newtype.unpack & fromIntegral & Binary.putWord64le

instance Newtype.Newtype Word64LE

instance DeepSeq.NFData Word64LE

instance Aeson.ToJSON Word64LE
