{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.BooleanSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Boolean" $ do
    it "can be decoded" $ do
        decodeBoolean "\0" `shouldBe` Right ("", 1, Boolean False)
        decodeBoolean "\1" `shouldBe` Right ("", 1, Boolean True)
    it "can be encoded" $ do
        Binary.encode (Boolean False) `shouldBe` "\0"
        Binary.encode (Boolean True) `shouldBe` "\1"
    it "does not raise a runtime error when decoding garbage" $ do
        decodeBoolean "garbage" `shouldBe` Left ("arbage", 1, "out of bounds")

decodeBoolean :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Boolean)
decodeBoolean = Binary.decodeOrFail
