{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Int64LESpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Int64LE" $ do
    it "can be decoded" $ do
        decodeInt64LE "\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 8, NewInt64LE 0)
        decodeInt64LE "\1\0\0\0\0\0\0\0" `shouldBe` Right ("", 8, NewInt64LE 1)
    it "can be encoded" $ do
        Binary.encode (NewInt64LE 0) `shouldBe` "\0\0\0\0\0\0\0\0"
        Binary.encode (NewInt64LE 1) `shouldBe` "\1\0\0\0\0\0\0\0"

decodeInt64LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int64LE)
decodeInt64LE = Binary.decodeOrFail
