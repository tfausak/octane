{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Vector
  ( Vector(..)
  , getFloatVector
  , getInt8Vector
  , getIntVector
  , putInt8Vector
  , putIntVector
  ) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Bits as Bits
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
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
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Vector)

instance (DeepSeq.NFData a) =>
         DeepSeq.NFData (Vector a)

-- | Encoded as a JSON array with 3 elements.
--
-- Aeson.encode (Vector 1 2 3 :: Vector Int)
-- "[1,2,3]"
instance (Aeson.ToJSON a) =>
         Aeson.ToJSON (Vector a) where
  toJSON vector = Aeson.toJSON [#x vector, #y vector, #z vector]

-- | Gets a 'Vector' full of 'Float's.
getFloatVector :: BinaryBit.BitGet (Vector Float)
getFloatVector = do
  let maxValue = 1
  let numBits = 16
  x <- getFloat maxValue numBits
  y <- getFloat maxValue numBits
  z <- getFloat maxValue numBits
  pure (Vector x y z)

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

-- | Gets a 'Vector' full of 'Int8's.
getInt8Vector :: BinaryBit.BitGet (Vector Int8.Int8)
getInt8Vector = do
  (hasX :: Boolean.Boolean) <- BinaryBit.getBits 0
  x <-
    if #unpack hasX
      then BinaryBit.getBits 0
      else pure 0
  (hasY :: Boolean.Boolean) <- BinaryBit.getBits 0
  y <-
    if #unpack hasY
      then BinaryBit.getBits 0
      else pure 0
  (hasZ :: Boolean.Boolean) <- BinaryBit.getBits 0
  z <-
    if #unpack hasZ
      then BinaryBit.getBits 0
      else pure 0
  pure (Vector x y z)

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
  pure (Vector (dx - bias) (dy - bias) (dz - bias))

-- | Puts a 'Vector' full of 'Int8's.
putInt8Vector :: Vector Int8.Int8 -> BinaryBit.BitPut ()
putInt8Vector _ = do
  pure () -- TODO

-- | Puts a 'Vector' full of 'Int's.
putIntVector :: Vector Int -> BinaryBit.BitPut ()
putIntVector _ = do
  pure () -- TODO
