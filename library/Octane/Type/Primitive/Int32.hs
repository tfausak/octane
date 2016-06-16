{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Int32 (Int32(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.Function ((&))
import qualified Data.Int as Int
import qualified GHC.Generics as Generics

-- | A 32-bit little-endian integer.
newtype Int32 = Int32
    { unpackInt32 :: Int.Int32
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Int32 where
    get = do
        word <- Binary.getWord32le
        word & fromIntegral & Int32 & return
    put word = do
        word & unpackInt32 & fromIntegral & Binary.putWord32le

instance DeepSeq.NFData Int32

instance Aeson.ToJSON Int32 where
    toJSON word = word & unpackInt32 & Aeson.toJSON
