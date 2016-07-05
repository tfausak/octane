{-# LANGUAGE BinaryLiterals #-}

module Octane.Utility.Endian
    ( reverseBitsInLazyBytes
    , reverseBitsInStrictBytes
    , reverseBits4
    , reverseBits20
    ) where

import qualified Data.Bits as Bits
import qualified Data.ByteString as StrictBytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Word as Word


-- | Reverses all the bits in each lazy byte.
--
-- >>> reverseBitsInLazyBytes "\x01"
-- "\128"
reverseBitsInLazyBytes :: LazyBytes.ByteString -> LazyBytes.ByteString
reverseBitsInLazyBytes bytes = LazyBytes.map reverseBits bytes


-- | Reverses all the bits in each strict byte.
--
-- >>> reverseBitsInStrictBytes "\x01"
-- "\128"
reverseBitsInStrictBytes :: StrictBytes.ByteString -> StrictBytes.ByteString
reverseBitsInStrictBytes bytes = StrictBytes.map reverseBits bytes


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


-- | Reverses the last 4 bits in a 1-byte word.
--
-- >>> reverseBits4 0xf3
-- 12
reverseBits4 :: Word.Word8 -> Word.Word8
reverseBits4 word
    = Bits.shiftR (word Bits..&. 0b1000) 3
    + Bits.shiftR (word Bits..&. 0b0100) 1
    + Bits.shiftL (word Bits..&. 0b0010) 1
    + Bits.shiftL (word Bits..&. 0b0001) 3


-- | Reverses the last 20 bits in a 4-byte word.
--
-- >>> reverseBits20 0xfff003ff
-- 1047552
reverseBits20 :: Word.Word32 -> Word.Word32
reverseBits20 word
    = Bits.shiftR (word Bits..&. 0b10000000000000000000) 19
    + Bits.shiftR (word Bits..&. 0b01000000000000000000) 17
    + Bits.shiftR (word Bits..&. 0b00100000000000000000) 15
    + Bits.shiftR (word Bits..&. 0b00010000000000000000) 13
    + Bits.shiftR (word Bits..&. 0b00001000000000000000) 11
    + Bits.shiftR (word Bits..&. 0b00000100000000000000) 9
    + Bits.shiftR (word Bits..&. 0b00000010000000000000) 7
    + Bits.shiftR (word Bits..&. 0b00000001000000000000) 5
    + Bits.shiftR (word Bits..&. 0b00000000100000000000) 3
    + Bits.shiftR (word Bits..&. 0b00000000010000000000) 1
    + Bits.shiftL (word Bits..&. 0b00000000001000000000) 1
    + Bits.shiftL (word Bits..&. 0b00000000000100000000) 3
    + Bits.shiftL (word Bits..&. 0b00000000000010000000) 5
    + Bits.shiftL (word Bits..&. 0b00000000000001000000) 7
    + Bits.shiftL (word Bits..&. 0b00000000000000100000) 9
    + Bits.shiftL (word Bits..&. 0b00000000000000010000) 11
    + Bits.shiftL (word Bits..&. 0b00000000000000001000) 13
    + Bits.shiftL (word Bits..&. 0b00000000000000000100) 15
    + Bits.shiftL (word Bits..&. 0b00000000000000000010) 17
    + Bits.shiftL (word Bits..&. 0b00000000000000000001) 19
