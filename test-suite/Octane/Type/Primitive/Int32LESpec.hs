{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Int32LESpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Int32LE" $ do
    it "can be decoded" $ do
        decodeInt32LE "\0\0\0\0" `shouldBe` Right ("", 4, NewInt32LE 0)
        decodeInt32LE "\1\0\0\0" `shouldBe` Right ("", 4, NewInt32LE 1)
    it "can be encoded" $ do
        Binary.encode (NewInt32LE 0) `shouldBe` "\0\0\0\0"
        Binary.encode (NewInt32LE 1) `shouldBe` "\1\0\0\0"

decodeInt32LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int32LE)
decodeInt32LE = Binary.decodeOrFail
