{-# LANGUAGE BinaryLiterals #-}
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

instance ToJSON Stream where
    toJSON stream =
        let size = stream & unpack & BS.length
            bytes = if size == 1 then "byte" else "bytes"
        in  toJSON (unwords ["Stream:", show size, bytes, "..."])

reverseBits :: Word.Word8 -> Word.Word8
reverseBits word
    = Bits.shiftR (word Bits..&. 0b10000000) 7
    + Bits.shiftR (word Bits..&. 0b01000000) 5
    + Bits.shiftR (word Bits..&. 0b00100000) 3
    + Bits.shiftR (word Bits..&. 0b00010000) 1
    + Bits.shiftL (word Bits..&. 0b00001000) 1
    + Bits.shiftL (word Bits..&. 0b00000100) 3
    + Bits.shiftL (word Bits..&. 0b00000010) 5
    + Bits.shiftL (word Bits..&. 0b00000001) 7
