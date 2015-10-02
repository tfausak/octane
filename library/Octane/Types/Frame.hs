module Octane.Types.Frame where

import Octane.Types.Float32LE (Float32LE)

import qualified Data.Binary as B
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
            { frameTime = B.decode (BSL.fromStrict timeBytes)
            , frameDelta = B.decode (BSL.fromStrict deltaBytes)
            }

    putBits _ frame = do
        BB.joinPut (do
            B.put (frameTime frame)
            B.put (frameDelta frame))
