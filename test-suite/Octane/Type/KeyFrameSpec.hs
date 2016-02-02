{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.KeyFrameSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "KeyFrame" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeKeyFrame "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0")
            (Right ("", 12, KeyFrame
                (Float32LE 0.0)
                (Int32LE 0)
                (Int32LE 0)))
        shouldBe
            (decodeKeyFrame "\
                \\0\0\128\63\
                \\2\0\0\0\
                \\3\0\0\0")
            (Right ("", 12, KeyFrame
                (Float32LE 1.0)
                (Int32LE 2)
                (Int32LE 3)))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (KeyFrame
                (Float32LE 0.0)
                (Int32LE 0)
                (Int32LE 0)))
            "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0"
        shouldBe
            (Binary.encode (KeyFrame
                (Float32LE 1.0)
                (Int32LE 2)
                (Int32LE 3)))
            "\
                \\0\0\128\63\
                \\2\0\0\0\
                \\3\0\0\0"

decodeKeyFrame :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, KeyFrame)
decodeKeyFrame = Binary.decodeOrFail
