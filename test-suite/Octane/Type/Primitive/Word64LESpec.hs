{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Word64LESpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Word64LE" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeWord64LE "\0\0\0\0\0\0\0\0")
            (Right ("", 8, Word64LE 0))
        shouldBe
            (decodeWord64LE "\1\0\0\0\0\0\0\0")
            (Right ("", 8, Word64LE 1))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Word64LE 0))
            "\0\0\0\0\0\0\0\0"
        shouldBe
            (Binary.encode (Word64LE 1))
            "\1\0\0\0\0\0\0\0"

decodeWord64LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Word64LE)
decodeWord64LE = Binary.decodeOrFail
