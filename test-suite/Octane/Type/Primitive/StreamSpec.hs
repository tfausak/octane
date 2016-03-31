{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.StreamSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Stream" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeStream "\0\0\0\0")
            (Right ("", 4, Stream ""))
    it "reverses the bits within each byte when decoding" $ do
        shouldBe
            (decodeStream "\2\0\0\0\128\64")
            (Right ("", 6, Stream "\1\2"))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Stream ""))
            "\0\0\0\0"
    it "reverses the bits within each byte when encoding" $ do
        shouldBe
            (Binary.encode (Stream "\1\2"))
            "\2\0\0\0\128\64"

decodeStream :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Stream)
decodeStream = Binary.decodeOrFail
