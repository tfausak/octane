{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Stream (Stream(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Bits as Bits
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Word32LE as Word32LE

-- | A length-prefixed stream of bits. The length is given in bytes. Each byte
-- | is reversed such that 0x01234567 is actually 0x76543210.
newtype Stream =
    Stream BS.ByteString
    deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Stream where
    get = do
        Word32LE.Word32LE size <- Binary.get
        content <- size & fromIntegral & Binary.getByteString
        content & BS.map reverseBits & Newtype.pack & return
    put stream = do
        let content = Newtype.unpack stream
        content & BS.length & fromIntegral & Word32LE.Word32LE & Binary.put
        content & BS.map reverseBits & Binary.putByteString

instance Newtype.Newtype Stream

instance DeepSeq.NFData Stream

instance Aeson.ToJSON Stream where
    toJSON stream =
        let size = stream & Newtype.unpack & BS.length
            bytes =
                if size == 1
                    then "byte"
                    else "bytes"
        in Aeson.toJSON (unwords ["Stream:", show size, bytes, "..."])

reverseBits :: Word.Word8 -> Word.Word8
reverseBits word =
    Bits.shiftR (word Bits..&. 128) 7 + Bits.shiftR (word Bits..&. 64) 5 +
    Bits.shiftR (word Bits..&. 32) 3 +
    Bits.shiftR (word Bits..&. 16) 1 +
    Bits.shiftL (word Bits..&. 8) 1 +
    Bits.shiftL (word Bits..&. 4) 3 +
    Bits.shiftL (word Bits..&. 2) 5 +
    Bits.shiftL (word Bits..&. 1) 7
