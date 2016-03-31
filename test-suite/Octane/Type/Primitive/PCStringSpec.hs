{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.PCStringSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "PCString" $ do
    it "can be decoded" $ do
        shouldBe
            (decodePCString "\1\0\0\0\0")
            (Right ("", 5, PCString ""))
        shouldBe
            (decodePCString "\6\0\0\0ascii\0")
            (Right ("", 10, PCString "ascii"))
        shouldBe
            (decodePCString "\8\0\0\0\251\241\239\231\248d\233\0")
            (Right ("", 12, PCString "ûñïçødé"))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (PCString ""))
            "\1\0\0\0\0"
        shouldBe
            (Binary.encode (PCString "ascii"))
            "\6\0\0\0ascii\0"
        shouldBe
            (Binary.encode (PCString "ûñïçødé"))
            "\8\0\0\0\251\241\239\231\248d\233\0"
    it "does not decode strings of length 0" $ do
        shouldBe
            (decodePCString "\0\0\0\0")
            (Left ("", 4, "invalid PCString size 0"))

decodePCString :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, PCString)
decodePCString = Binary.decodeOrFail
