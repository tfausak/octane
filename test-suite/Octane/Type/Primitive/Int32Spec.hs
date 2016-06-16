{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Int32Spec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Int32" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeInt32 "\0\0\0\0")
            (Right ("", 4, Int32 0))
        shouldBe
            (decodeInt32 "\1\0\0\0")
            (Right ("", 4, Int32 1))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Int32 0))
            "\0\0\0\0"
        shouldBe
            (Binary.encode (Int32 1))
            "\1\0\0\0"

decodeInt32 :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int32)
decodeInt32 = Binary.decodeOrFail
