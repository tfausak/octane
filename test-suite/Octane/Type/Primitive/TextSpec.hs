{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.TextSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Text" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeText "\1\0\0\0\0")
            (Right ("", 5, Text ""))
        shouldBe
            (decodeText "\6\0\0\0ascii\0")
            (Right ("", 10, Text "ascii"))
        shouldBe
            (decodeText "\254\255\255\255\24\35\0\0")
            (Right ("", 8, Text "\8984"))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Text ""))
            "\1\0\0\0\0"
        shouldBe
            (Binary.encode (Text "ascii"))
            "\6\0\0\0ascii\0"
        shouldBe
            (Binary.encode (Text "\8984"))
            "\254\255\255\255\24\35\0\0"
    it "does not decode strings of length 0" $ do
        shouldBe
            (decodeText "\0\0\0\0")
            (Left ("", 4, "Unexpected Text size 0"))

decodeText :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Text)
decodeText = Binary.decodeOrFail
