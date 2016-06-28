{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Vector (Vector(..), getFloatVector, getIntVector) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Bits as Bits
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified GHC.Generics as Generics
import qualified Octane.Type.CompressedWord as CompressedWord


-- | Three values packed together. Although the fields are called @x@, @y@, and
-- @z@, that may not be what they actually represent.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it is not
-- always serialized the same way. Sometimes it is three values run together,
-- but other times it has a flag for the presence of each value.
data Vector a = Vector
    { x :: a
    , y :: a
    , z :: a
    } deriving (Eq, Generics.Generic, Show)

instance (DeepSeq.NFData a) => DeepSeq.NFData (Vector a) where

-- | Encoded as a JSON array with 3 elements.
--
-- Aeson.encode (Vector 1 2 3 :: Vector Int)
-- "[1,2,3]"
instance (Aeson.ToJSON a) => Aeson.ToJSON (Vector a) where
    toJSON vector = Aeson.toJSON [x vector, y vector, z vector]


-- | Gets a 'Vector' full of 'Float's.
getFloatVector :: BinaryBit.BitGet (Vector Float)
getFloatVector = do
    let maxValue = 1
    let numBits = 16

    x' <- getFloat maxValue numBits
    y' <- getFloat maxValue numBits
    z' <- getFloat maxValue numBits

    pure Vector { x = x', y = y', z = z' }


getFloat :: Int -> Int -> BinaryBit.BitGet Float
getFloat maxValue numBits = do
    let maxBitValue = (Bits.shiftL 1 (numBits - 1)) - 1
    let bias = Bits.shiftL 1 (numBits - 1)
    let serIntMax = Bits.shiftL 1 numBits
    delta <- fmap CompressedWord.fromCompressedWord (BinaryBit.getBits serIntMax)
    let unscaledValue = (delta :: Int) - bias
    if maxValue > maxBitValue
    then do
        let invScale = fromIntegral maxValue / fromIntegral maxBitValue
        pure (fromIntegral unscaledValue * invScale)
    else do
        let scale = fromIntegral maxBitValue / fromIntegral maxValue
        let invScale = 1.0 / scale
        pure (fromIntegral unscaledValue * invScale)


-- | Gets a 'Vector' full of 'Int's.
getIntVector :: BinaryBit.BitGet (Vector Int)
getIntVector = do
    numBits <- fmap CompressedWord.fromCompressedWord (BinaryBit.getBits 19)
    let bias = Bits.shiftL 1 (numBits + 1)
    let maxBits = numBits + 2
    let maxValue = 2 ^ maxBits

    dx <- fmap CompressedWord.fromCompressedWord (BinaryBit.getBits maxValue)
    dy <- fmap CompressedWord.fromCompressedWord (BinaryBit.getBits maxValue)
    dz <- fmap CompressedWord.fromCompressedWord (BinaryBit.getBits maxValue)

    pure Vector { x = dx - bias , y = dy - bias , z = dz - bias }
