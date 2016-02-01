{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.FrameSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Frame" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeFrame "\
                \\0\0\0\0\
                \\0\0\0\0\
                \")
            (Right ("", 8, Frame
                (Float32LE 0.0)
                (Float32LE 0.0)))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Frame
                (Float32LE 0.0)
                (Float32LE 0.0)))
            "\
                \\0\0\0\0\
                \\0\0\0\0\
                \"

decodeFrame :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Frame)
decodeFrame = Binary.decodeOrFail
