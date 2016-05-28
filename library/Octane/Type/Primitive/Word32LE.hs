{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Word32LE (Word32LE(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.Function ((&))
import qualified Data.Int as Int
import qualified GHC.Generics as Generics

-- | A 32-bit little-endian integer.
newtype Word32LE =
    Word32LE Int.Int32
    deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Word32LE where
    get = do
        word <- Binary.getWord32le
        word & fromIntegral & Newtype.pack & return
    put word = do
        word & Newtype.unpack & fromIntegral & Binary.putWord32le

instance Newtype.Newtype Word32LE

instance DeepSeq.NFData Word32LE

instance Aeson.ToJSON Word32LE
