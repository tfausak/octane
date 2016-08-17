module Octane.Utility.Endian
    ( reverseBitsInLazyBytes
    , reverseBitsInStrictBytes
    ) where

import Basics

import qualified Data.Bits as Bits
import qualified Data.ByteString as StrictBytes
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Word as Word


-- | Reverses all the bits in each lazy byte.
reverseBitsInLazyBytes :: LazyBytes -> LazyBytes
reverseBitsInLazyBytes bytes = LazyBytes.map reverseBits bytes


-- | Reverses all the bits in each strict byte.
reverseBitsInStrictBytes :: StrictBytes -> StrictBytes
reverseBitsInStrictBytes bytes = StrictBytes.map reverseBits bytes


reverseBits :: Word.Word8 -> Word.Word8
reverseBits byte
    = Bits.shiftR (byte Bits..&. 0b10000000) 7
    + Bits.shiftR (byte Bits..&. 0b01000000) 5
    + Bits.shiftR (byte Bits..&. 0b00100000) 3
    + Bits.shiftR (byte Bits..&. 0b00010000) 1
    + Bits.shiftL (byte Bits..&. 0b00001000) 1
    + Bits.shiftL (byte Bits..&. 0b00000100) 3
    + Bits.shiftL (byte Bits..&. 0b00000010) 5
    + Bits.shiftL (byte Bits..&. 0b00000001) 7
