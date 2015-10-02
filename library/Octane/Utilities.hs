module Octane.Utilities where

import Data.Bits (setBit, testBit, zeroBits)

import qualified Data.ByteString as BS

flipEndianness :: BS.ByteString -> BS.ByteString
flipEndianness bytes = BS.map go bytes where
    go byte =
        let a = testBit byte 0
            b = testBit byte 1
            c = testBit byte 2
            d = testBit byte 3
            e = testBit byte 4
            f = testBit byte 5
            g = testBit byte 6
            h = testBit byte 7
        in  (if a then flip setBit 7 else id) $
            (if b then flip setBit 6 else id) $
            (if c then flip setBit 5 else id) $
            (if d then flip setBit 4 else id) $
            (if e then flip setBit 3 else id) $
            (if f then flip setBit 2 else id) $
            (if g then flip setBit 1 else id) $
            (if h then flip setBit 0 else id) $
            zeroBits
