module Octane.Type.Vector
    ( Vector(..)
    , getFloatVector
    , getInt8Vector
    , getIntVector
    , putInt8Vector
    , putIntVector
    ) where

import Basics

import qualified Data.Bits as Bits
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Int8 as Int8


-- | Three values packed together. Although the fields are called @x@, @y@, and
-- @z@, that may not be what they actually represent.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it is not
-- always serialized the same way. Sometimes it is three values run together,
-- but other times it has a flag for the presence of each value.
data Vector a = Vector
    { vectorX :: a
    , vectorY :: a
    , vectorZ :: a
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''Vector)

instance (NFData a) => NFData (Vector a) where

-- | Encoded as a JSON array with 3 elements.
--
-- Aeson.encode (Vector 1 2 3 :: Vector Int)
-- "[1,2,3]"
instance (ToJSON a) => ToJSON (Vector a) where
    toJSON vector = toJSON [#x vector, #y vector, #z vector]


-- | Gets a 'Vector' full of 'Float's.
getFloatVector :: BitGet (Vector Float)
getFloatVector = do
    let maxValue = 1
    let numBits = 16

    x <- getFloat maxValue numBits
    y <- getFloat maxValue numBits
    z <- getFloat maxValue numBits

    pure (Vector x y z)


getFloat :: Int -> Int -> BitGet Float
getFloat maxValue numBits = do
    let maxBitValue = (Bits.shiftL 1 (numBits - 1)) - 1
    let bias = Bits.shiftL 1 (numBits - 1)
    let serIntMax = Bits.shiftL 1 numBits
    delta <- fmap CompressedWord.fromCompressedWord (getBits serIntMax)
    let unscaledValue = (delta :: Int) - bias
    if maxValue > maxBitValue
    then do
        let invScale = fromIntegral maxValue / fromIntegral maxBitValue
        pure (fromIntegral unscaledValue * invScale)
    else do
        let scale = fromIntegral maxBitValue / fromIntegral maxValue
        let invScale = 1.0 / scale
        pure (fromIntegral unscaledValue * invScale)


-- | Gets a 'Vector' full of 'Int8's.
getInt8Vector :: BitGet (Vector Int8.Int8)
getInt8Vector = do
    (hasX :: Boolean.Boolean) <- getBits 0
    x <- if #unpack hasX then getBits 0 else pure 0

    (hasY :: Boolean.Boolean) <- getBits 0
    y <- if #unpack hasY then getBits 0 else pure 0

    (hasZ :: Boolean.Boolean) <- getBits 0
    z <- if #unpack hasZ then getBits 0 else pure 0

    pure (Vector x y z)


-- | Gets a 'Vector' full of 'Int's.
getIntVector :: BitGet (Vector Int)
getIntVector = do
    numBits <- fmap CompressedWord.fromCompressedWord (getBits 19)
    let bias = Bits.shiftL 1 (numBits + 1)
    let maxBits = numBits + 2
    let maxValue = 2 ^ maxBits

    dx <- fmap CompressedWord.fromCompressedWord (getBits maxValue)
    dy <- fmap CompressedWord.fromCompressedWord (getBits maxValue)
    dz <- fmap CompressedWord.fromCompressedWord (getBits maxValue)

    pure (Vector (dx - bias) (dy - bias) (dz - bias))


-- | Puts a 'Vector' full of 'Int8's.
putInt8Vector :: Vector Int8.Int8 -> BitPut ()
putInt8Vector _ = do
    pure ()


-- | Puts a 'Vector' full of 'Int's.
putIntVector :: Vector Int -> BitPut ()
putIntVector _ = do
    pure ()
