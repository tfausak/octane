{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Int64 (Int64(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.Function ((&))
import qualified Data.Int as Int
import qualified GHC.Generics as Generics

-- | A 64-bit little-endian integer.
newtype Int64 = Int64
    { unpackInt64 :: Int.Int64
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Int64 where
    get = do
        word <- Binary.getWord64le
        word & fromIntegral & Int64 & return
    put word = do
        word & unpackInt64 & fromIntegral & Binary.putWord64le

instance DeepSeq.NFData Int64

instance Aeson.ToJSON Int64 where
    toJSON word = word & unpackInt64 & Aeson.toJSON
