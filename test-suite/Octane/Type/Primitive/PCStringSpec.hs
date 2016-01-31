{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.PCStringSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "PCString" $ do
    it "can be decoded" $ do
        decodePCString "\1\0\0\0\0" `shouldBe` Right ("", 5, NewPCString "")
        decodePCString "\6\0\0\0ascii\0" `shouldBe` Right ("", 10, NewPCString "ascii")
        decodePCString "\8\0\0\0\251\241\239\231\248d\233\0" `shouldBe` Right ("", 12, NewPCString "ûñïçødé")
    it "can be encoded" $ do
        Binary.encode (NewPCString "") `shouldBe` "\1\0\0\0\0"
        Binary.encode (NewPCString "ascii") `shouldBe` "\6\0\0\0ascii\0"
        Binary.encode (NewPCString "ûñïçødé") `shouldBe` "\8\0\0\0\251\241\239\231\248d\233\0"
    it "does not decode strings of length 0" $ do
        decodePCString "\0\0\0\0" `shouldBe` Left ("", 4, "invalid size")

decodePCString :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, PCString)
decodePCString = Binary.decodeOrFail
