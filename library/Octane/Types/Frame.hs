module Octane.Types.Frame where

import Octane.Types.Float32LE (Float32LE)

import qualified Data.Binary as B
import qualified Data.Binary.Bits as Bit
import qualified Data.Binary.Bits.Get as Bit
import qualified Data.Binary.Bits.Put as Bit
import qualified Data.ByteString.Lazy as BSL

data Frame = NewFrame
    { frameTime :: Float32LE
    , frameDelta :: Float32LE
    } deriving (Show)

instance Bit.BinaryBit Frame where
    getBits _ = do
        timeBytes <- Bit.getByteString 4
        deltaBytes <- Bit.getByteString 4
        return NewFrame
            { frameTime = B.decode (BSL.fromStrict timeBytes)
            , frameDelta = B.decode (BSL.fromStrict deltaBytes)
            }

    putBits _ frame = do
        Bit.joinPut (do
            B.put (frameTime frame)
            B.put (frameDelta frame))
