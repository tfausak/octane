{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Word32 (Word32(..), fromWord32) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Scientific as Scientific
import qualified Data.Word as Word
import qualified GHC.Generics as Generics


-- | A 32-bit little-endian word.
newtype Word32 = Word32
    { unpackWord32 :: Word.Word32
    } deriving (Eq, Generics.Generic, Num, Ord, Show)

instance Binary.Binary Word32 where
    get = do
        value <- Binary.getWord32le
        value & Word32 & pure

    put word32 = word32
        & unpackWord32
        & Binary.putWord32le

instance Aeson.FromJSON Word32 where
    parseJSON json = case json of
        Aeson.Number number -> case Scientific.toBoundedInteger number of
            Just integer -> pure (Word32 integer)
            _ -> Aeson.typeMismatch "Word32" json
        _ -> Aeson.typeMismatch "Word32" json

instance DeepSeq.NFData Word32 where

instance Aeson.ToJSON Word32 where
    toJSON word32 = word32
        & unpackWord32
        & Aeson.toJSON


fromWord32 :: (Integral a) => Word32 -> a
fromWord32 word32 = word32 & unpackWord32 & fromIntegral
