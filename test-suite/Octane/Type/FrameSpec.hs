{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.FrameSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary.Bits as Bits
import qualified Data.Binary.Bits.Get as Bits
import qualified Data.Binary.Bits.Put as Bits
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Frame" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeFrame "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0")
            (Right ("", 9, Frame
                (Float32LE 0.0)
                (Float32LE 0.0)
                []))
    it "can be encoded" $ do
        shouldBe
            (encodeFrame (Frame
                (Float32LE 0.0)
                (Float32LE 0.0)
                []))
            "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0"

decodeFrame :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Frame)
decodeFrame bytes = Binary.runGetOrFail (Bits.runBitGet (Bits.getBits undefined)) bytes

encodeFrame :: Frame -> BSL.ByteString
encodeFrame frame = Binary.runPut (Bits.runBitPut (Bits.putBits undefined frame))
