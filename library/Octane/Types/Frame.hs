module Octane.Types.Frame where

import Octane.Types.Float32LE (Float32LE)
import Octane.Utilities (flipEndianness)

import qualified Data.Binary as B
import qualified Data.Binary.Put as B
import qualified Data.Binary.Bits as BB
import qualified Data.Binary.Bits.Get as BB
import qualified Data.Binary.Bits.Put as BB
import qualified Data.ByteString.Lazy as BSL

data Frame = NewFrame
    { frameTime :: Float32LE
    , frameDelta :: Float32LE
    } deriving (Show)

instance BB.BinaryBit Frame where
    getBits _ = do
        timeBytes <- BB.getByteString 4
        deltaBytes <- BB.getByteString 4
        return NewFrame
            { frameTime = B.decode (BSL.fromStrict (flipEndianness timeBytes))
            , frameDelta = B.decode (BSL.fromStrict (flipEndianness deltaBytes))
            }

    putBits _ frame = do
        let timeBytes = flipEndianness (BSL.toStrict (B.encode (frameTime frame)))
            deltaBytes = flipEndianness (BSL.toStrict (B.encode (frameDelta frame)))
        BB.joinPut (do
            B.putByteString timeBytes
            B.putByteString deltaBytes)
