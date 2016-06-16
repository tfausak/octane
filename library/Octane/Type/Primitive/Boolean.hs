{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Boolean (Boolean(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified GHC.Generics as Generics


-- | A boolean value.
newtype Boolean = Boolean
    { unpackBoolean :: Bool
    } deriving (Eq, Generics.Generic, Show)

-- | Boolean values are stored in the last bit of a byte. Decoding will fail if
-- the byte is anything other than @0b00000000@ or @0b00000001@.
instance Binary.Binary Boolean where
    get = do
        value <- Binary.getWord8
        case value of
            0 -> pure (Boolean False)
            1 -> pure (Boolean True)
            _ -> fail ("Unexpected Boolean value " ++ show value)

    put boolean = boolean
        & unpackBoolean
        & fromEnum
        & fromIntegral
        & Binary.putWord8

-- | Boolean values are stored as a single bit.
instance BinaryBit.BinaryBit Boolean where
    getBits _ = do
        value <- BinaryBit.getBool
        pure (Boolean value)

    putBits _ boolean = boolean
        & unpackBoolean
        & BinaryBit.putBool

-- | Boolean values are decoded directly from @true@ or @false@.
instance Aeson.FromJSON Boolean where
    parseJSON json = case json of
        Aeson.Bool value -> pure (Boolean value)
        _ -> Aeson.typeMismatch "Boolean" json

instance DeepSeq.NFData Boolean where

-- | Boolean values are encoded directly as @true@ or @false@.
instance Aeson.ToJSON Boolean where
    toJSON boolean = boolean
        & unpackBoolean
        & Aeson.toJSON
