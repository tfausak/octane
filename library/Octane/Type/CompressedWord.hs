module Octane.Type.CompressedWord (CompressedWord(..), fromCompressedWord) where

import Basics

import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Bits as Bits
import qualified Octane.Type.Boolean as Boolean


-- | A compressed, unsigned integer. When serialized, the least significant bit
-- is first. Bits are serialized until the next bit would be greater than the
-- limit, or the number of bits necessary to reach the limit has been reached,
-- whichever comes first.
data CompressedWord = CompressedWord
    { compressedWordLimit :: Word
    , compressedWordValue :: Word
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''CompressedWord)

-- | Abuses the first argument to 'BinaryBit.getBits' as the maximum value.
instance BinaryBit CompressedWord where
    getBits n = do
        let limit = fromIntegral n
        value <- getStep limit (bitSize limit) 0 0
        pure (CompressedWord limit value)

    putBits _ compressedWord = do
        let limit = fromIntegral (#limit compressedWord)
        let value = fromIntegral (#value compressedWord)
        let maxBits = bitSize limit
        let upper = (2 ^ (maxBits - 1)) - 1
        let lower = limit - upper
        let numBits = if lower > value || value > upper
                then maxBits
                else maxBits - 1
        BinaryBit.putWord64be numBits value

instance NFData CompressedWord where

-- | Encoded as an object.
instance ToJSON CompressedWord where
    toJSON compressedWord = Aeson.object
        [ "Limit" .= #limit compressedWord
        , "Value" .= #value compressedWord
        ]


-- | Converts a 'CompressedWord' into any integral value. This is a lossy
-- conversion because it discards the compressed word's maximum value.
fromCompressedWord :: (Integral a) => CompressedWord -> a
fromCompressedWord compressedWord = compressedWord & #value & fromIntegral


bitSize :: (Integral a, Integral b) => a -> b
bitSize x = x & fromIntegral & logBase (2 :: Double) & ceiling


getStep :: Word -> Word -> Word -> Word -> BitGet Word
getStep limit maxBits position value = do
    let x = Bits.shiftL 1 (fromIntegral position)
    if position < maxBits && value + x <= limit
    then do
        (bit :: Boolean.Boolean) <- BinaryBit.getBits 0
        let newValue = if #unpack bit then value + x else value
        getStep limit maxBits (position + 1) newValue
    else pure value
