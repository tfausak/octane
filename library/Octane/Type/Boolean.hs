module Octane.Type.Boolean (Boolean(..)) where

import Basics

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit


-- | A boolean value.
newtype Boolean = Boolean
    { booleanUnpack :: Bool
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''Boolean)

-- | Stored in the last bit of a byte. Decoding will fail if the byte is
-- anything other than @0b00000000@ or @0b00000001@.
instance Binary Boolean where
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
instance BinaryBit Boolean where
    getBits _ = do
        value <- BinaryBit.getBool
        value & Boolean & pure

    putBits _ boolean = boolean
        & #unpack
        & BinaryBit.putBool

instance NFData Boolean where

-- | Encoded directly as a JSON boolean.
instance ToJSON Boolean where
    toJSON boolean = boolean
        & #unpack
        & toJSON
