{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.BooleanSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Octane as Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Boolean" $ do
    it "can be decoded" $ do
        decodeBoolean "\0" `shouldBe` Right ("", 1, Octane.NewBoolean False)
        decodeBoolean "\1" `shouldBe` Right ("", 1, Octane.NewBoolean True)
    it "can be encoded" $ do
        Binary.encode (Octane.NewBoolean False) `shouldBe` "\0"
        Binary.encode (Octane.NewBoolean True) `shouldBe` "\1"
    it "handles extra data when decoding" $ do
        decodeBoolean "\0extra" `shouldBe` Right ("extra", 1, Octane.NewBoolean False)
    it "does not raise a runtime error when decoding garbage" $ do
        decodeBoolean "garbage" `shouldBe` Left ("arbage", 1, "out of bounds")

decodeBoolean :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Octane.Boolean)
decodeBoolean = Binary.decodeOrFail
