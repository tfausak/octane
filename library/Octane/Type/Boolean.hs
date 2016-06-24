{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Octane.Type.Boolean (Boolean(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified GHC.Generics as Generics

-- $setup
-- >>> import qualified Data.Binary.Get as Binary
-- >>> import qualified Data.Binary.Put as Binary


-- | A boolean value.
newtype Boolean = Boolean
    { unpack :: Bool
    } deriving (Eq, Generics.Generic, Show)

-- | Stored in the last bit of a byte. Decoding will fail if the byte is
-- anything other than @0b00000000@ or @0b00000001@.
--
-- >>> Binary.decode "\x01" :: Boolean
-- Boolean {unpack = True}
--
-- >>> Binary.encode (Boolean True)
-- "\SOH"
instance Binary.Binary Boolean where
    get = do
        value <- Binary.getWord8
        case value of
            0 -> pure (Boolean False)
            1 -> pure (Boolean True)
            _ -> fail ("Unexpected Boolean value " ++ show value)

    put boolean = boolean
        & unpack
        & fromEnum
        & fromIntegral
        & Binary.putWord8

-- | Stored as a bit.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits undefined)) "\x80" :: Boolean
-- Boolean {unpack = True}
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits undefined (Boolean True)))
-- "\128"
instance BinaryBit.BinaryBit Boolean where
    getBits _ = do
        value <- BinaryBit.getBool
        value & Boolean & pure

    putBits _ boolean = boolean
        & unpack
        & BinaryBit.putBool

instance DeepSeq.NFData Boolean where

-- | Encoded directly as a JSON boolean.
--
-- >>> Aeson.encode (Boolean True)
-- "true"
instance Aeson.ToJSON Boolean where
    toJSON boolean = boolean
        & unpack
        & Aeson.toJSON
