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
        shouldBe
            (decodeInt32LE "\0\0\0\0")
            (Right ("", 4, Int32LE 0))
        shouldBe
            (decodeInt32LE "\1\0\0\0")
            (Right ("", 4, Int32LE 1))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Int32LE 0))
            "\0\0\0\0"
        shouldBe
            (Binary.encode (Int32LE 1))
            "\1\0\0\0"

decodeInt32LE :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Int32LE)
decodeInt32LE = Binary.decodeOrFail
