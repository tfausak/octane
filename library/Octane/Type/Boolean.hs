{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Octane.Type.Boolean (Boolean(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics

-- $setup
-- >>> import qualified Data.Binary.Get as Binary
-- >>> import qualified Data.Binary.Put as Binary


-- | A boolean value.
newtype Boolean = Boolean
    { booleanUnpack :: Bool
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Boolean)

-- | Stored in the last bit of a byte. Decoding will fail if the byte is
-- anything other than @0b00000000@ or @0b00000001@.
--
-- >>> Binary.decode "\x01" :: Boolean
-- Boolean {booleanUnpack = True}
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
        & #unpack
        & fromEnum
        & fromIntegral
        & Binary.putWord8

-- | Stored as a bit.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x80" :: Boolean
-- Boolean {booleanUnpack = True}
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (Boolean True)))
-- "\128"
instance BinaryBit.BinaryBit Boolean where
    getBits _ = do
        value <- BinaryBit.getBool
        value & Boolean & pure

    putBits _ boolean = boolean
        & #unpack
        & BinaryBit.putBool

instance DeepSeq.NFData Boolean where

-- | Encoded directly as a JSON boolean.
--
-- >>> Aeson.encode (Boolean True)
-- "true"
instance Aeson.ToJSON Boolean where
    toJSON boolean = boolean
        & #unpack
        & Aeson.toJSON
