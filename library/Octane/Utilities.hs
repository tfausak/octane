module Octane.Utilities where

import Data.Bits (setBit, testBit)

import qualified Data.Binary as B
import qualified Data.ByteString as BS

flipEndianness :: BS.ByteString -> BS.ByteString
flipEndianness bytes = BS.map (toWord . reverse . toBits) bytes

toBits :: B.Word8 -> [Bool]
toBits word = map (testBit word) [0 .. 7]

toWord :: [Bool] -> B.Word8
toWord bits = foldl
    (\ word (index, bit) -> if bit then setBit word index else word)
    0
    (zip [0 .. 7] bits)
