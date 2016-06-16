{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Int64Spec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Int64" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeInt64 "\0\0\0\0\0\0\0\0")
            (Right ("", 8, Int64 0))
        shouldBe
            (decodeInt64 "\1\0\0\0\0\0\0\0")
            (Right ("", 8, Int64 1))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Int64 0))
            "\0\0\0\0\0\0\0\0"
        shouldBe
            (Binary.encode (Int64 1))
            "\1\0\0\0\0\0\0\0"

decodeInt64 :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int64)
decodeInt64 = Binary.decodeOrFail
