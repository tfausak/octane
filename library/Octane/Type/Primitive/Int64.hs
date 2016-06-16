{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Int64 (Int64(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Int as Int
import qualified Data.Scientific as Scientific
import qualified GHC.Generics as Generics


-- | A 64-bit little-endian integer.
newtype Int64 = Int64
    { unpackInt64 :: Int.Int64
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Int64 where
    get = do
        value <- Binary.getWord64le
        value & fromIntegral & Int64 & return

    put word64 = word64
        & unpackInt64
        & fromIntegral
        & Binary.putWord64le

instance Aeson.FromJSON Int64 where
    parseJSON json = case json of
        Aeson.Number number -> case Scientific.toBoundedInteger number of
            Just integer -> pure (Int64 integer)
            _ -> Aeson.typeMismatch "Int64" json
        _ -> Aeson.typeMismatch "Int64" json

instance DeepSeq.NFData Int64

instance Aeson.ToJSON Int64 where
    toJSON int64 = int64
        & unpackInt64
        & Aeson.toJSON
