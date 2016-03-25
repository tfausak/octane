{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Stream (Stream(..)) where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import qualified Data.Word as Word
import Octane.Core
import Octane.Type.Primitive.Word32LE

newtype Stream = Stream ByteString
    deriving (Eq, Generic, NFData, Show)

instance Binary Stream where
    get = do
        Word32LE size <- get
        content <- size & fromIntegral & Binary.getByteString
        content & BS.map reverseBits & pack & return

    put stream = do
        let content = stream & unpack
        content & BS.length & fromIntegral & Word32LE & put
        content & BS.map reverseBits & Binary.putByteString

instance Newtype Stream

reverseBits :: Word.Word8 -> Word.Word8
reverseBits word =
    (if Bits.testBit word 0 then 128 else 0) Bits..|.
    (if Bits.testBit word 1 then 64 else 0) Bits..|.
    (if Bits.testBit word 2 then 32 else 0) Bits..|.
    (if Bits.testBit word 3 then 16 else 0) Bits..|.
    (if Bits.testBit word 4 then 8 else 0) Bits..|.
    (if Bits.testBit word 5 then 4 else 0) Bits..|.
    (if Bits.testBit word 6 then 2 else 0) Bits..|.
    (if Bits.testBit word 7 then 1 else 0)
