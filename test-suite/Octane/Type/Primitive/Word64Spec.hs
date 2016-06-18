{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Word64Spec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Word64" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeWord64 "\0\0\0\0\0\0\0\0")
            (Right ("", 8, Word64 0))
        shouldBe
            (decodeWord64 "\1\0\0\0\0\0\0\0")
            (Right ("", 8, Word64 1))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Word64 0))
            "\0\0\0\0\0\0\0\0"
        shouldBe
            (Binary.encode (Word64 1))
            "\1\0\0\0\0\0\0\0"

decodeWord64 :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Word64)
decodeWord64 = Binary.decodeOrFail
