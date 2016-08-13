{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Octane.Type.CompressedWord (CompressedWord(..), fromCompressedWord) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Bits as Bits
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean

-- $setup
-- >>> import qualified Data.Binary.Get as Binary
-- >>> import qualified Data.Binary.Put as Binary


-- | A compressed, unsigned integer. When serialized, the least significant bit
-- is first. Bits are serialized until the next bit would be greater than the
-- limit, or the number of bits necessary to reach the limit has been reached,
-- whichever comes first.
data CompressedWord = CompressedWord
    { limit :: Word
    , value :: Word
    } deriving (Eq, Generics.Generic, Show)

-- | Abuses the first argument to 'BinaryBit.getBits' as the maximum value.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 4)) "\x7f" :: CompressedWord
-- CompressedWord {limit = 4, value = 2}
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 (CompressedWord 4 2)))
-- "\128"
instance BinaryBit.BinaryBit CompressedWord where
    getBits n = do
        let theLimit = fromIntegral n
        theValue <- getStep theLimit (bitSize theLimit) 0 0
        pure (CompressedWord theLimit theValue)

    putBits _ compressedWord = do
        let theLimit = fromIntegral (limit compressedWord)
        let theValue = fromIntegral (value compressedWord)
        let maxBits = bitSize theLimit
        let upper = (2 ^ (maxBits - 1)) - 1
        let lower = theLimit - upper
        let numBits = if lower > theValue || theValue > upper
                then maxBits
                else maxBits - 1
        BinaryBit.putWord64be numBits theValue

instance DeepSeq.NFData CompressedWord where

-- | Encoded as an object.
--
-- >>> Aeson.encode (CompressedWord 2 1)
-- "{\"Value\":1,\"Limit\":2}"
instance Aeson.ToJSON CompressedWord where
    toJSON compressedWord = Aeson.object
        [ "Limit" .= limit compressedWord
        , "Value" .= value compressedWord
        ]


-- | Converts a 'CompressedWord' into any integral value. This is a lossy
-- conversion because it discards the compressed word's maximum value.
--
-- >>> fromCompressedWord (CompressedWord 2 1) :: Int
-- 1
fromCompressedWord :: (Integral a) => CompressedWord -> a
fromCompressedWord compressedWord = compressedWord & value & fromIntegral


bitSize :: (Integral a, Integral b) => a -> b
bitSize x = x & fromIntegral & logBase (2 :: Double) & ceiling


getStep :: Word -> Word -> Word -> Word -> BinaryBit.BitGet Word
getStep theLimit maxBits position theValue = do
    let x = Bits.shiftL 1 (fromIntegral position)
    if position < maxBits && theValue + x <= theLimit
    then do
        (bit :: Boolean.Boolean) <- BinaryBit.getBits 0
        let newValue = if #unpack bit then theValue + x else theValue
        getStep theLimit maxBits (position + 1) newValue
    else pure theValue
