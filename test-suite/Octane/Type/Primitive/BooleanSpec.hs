{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.BooleanSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Boolean" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeBoolean "\0")
            (Right ("", 1, Boolean False))
        shouldBe
            (decodeBoolean "\1")
            (Right ("", 1, Boolean True))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Boolean False))
            "\0"
        shouldBe
            (Binary.encode (Boolean True))
            "\1"
    it "does not raise a runtime error when decoding garbage" $ do
        shouldBe
            (decodeBoolean "garbage")
            (Left ("arbage", 1, "invalid Boolean value 103"))

decodeBoolean :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Boolean)
decodeBoolean = Binary.decodeOrFail
