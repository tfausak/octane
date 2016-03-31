{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Word32LESpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Word32LE" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeWord32LE "\0\0\0\0")
            (Right ("", 4, Word32LE 0))
        shouldBe
            (decodeWord32LE "\1\0\0\0")
            (Right ("", 4, Word32LE 1))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Word32LE 0))
            "\0\0\0\0"
        shouldBe
            (Binary.encode (Word32LE 1))
            "\1\0\0\0"

decodeWord32LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Word32LE)
decodeWord32LE = Binary.decodeOrFail
