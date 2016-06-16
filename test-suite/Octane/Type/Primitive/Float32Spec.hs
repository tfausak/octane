{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Float32Spec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Float32" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeFloat32 "\0\0\0\0")
            (Right ("", 4, Float32 0.0))
        shouldBe
            (decodeFloat32 "\0\0\128\63")
            (Right ("", 4, Float32 1.0))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Float32 0.0))
            "\0\0\0\0"
        shouldBe
            (Binary.encode (Float32 1.0))
            "\0\0\128\63"

decodeFloat32 :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Float32)
decodeFloat32 = Binary.decodeOrFail
