{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.KeyFrameSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "KeyFrame" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeKeyFrame "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0")
            (Right ("", 12, KeyFrame
                (Float32 0.0)
                (Word32 0)
                (Word32 0)))
        shouldBe
            (decodeKeyFrame "\
                \\0\0\128\63\
                \\2\0\0\0\
                \\3\0\0\0")
            (Right ("", 12, KeyFrame
                (Float32 1.0)
                (Word32 2)
                (Word32 3)))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (KeyFrame
                (Float32 0.0)
                (Word32 0)
                (Word32 0)))
            "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0"
        shouldBe
            (Binary.encode (KeyFrame
                (Float32 1.0)
                (Word32 2)
                (Word32 3)))
            "\
                \\0\0\128\63\
                \\2\0\0\0\
                \\3\0\0\0"

decodeKeyFrame :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, KeyFrame)
decodeKeyFrame = Binary.decodeOrFail
