{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Float32LESpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Float32LE" $ do
    it "can be decoded" $ do
        decodeFloat32LE "\0\0\0\0" `shouldBe` Right ("", 4, Float32LE 0.0)
        decodeFloat32LE "\0\0\128\63" `shouldBe` Right ("", 4, Float32LE 1.0)
    it "can be encoded" $ do
        Binary.encode (Float32LE 0.0) `shouldBe` "\0\0\0\0"
        Binary.encode (Float32LE 1.0) `shouldBe` "\0\0\128\63"

decodeFloat32LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Float32LE)
decodeFloat32LE = Binary.decodeOrFail
