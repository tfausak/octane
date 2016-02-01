{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.PropertySpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Property" $ do
    it "can decode arrays" $ do
        decodeProperty "\14\0\0\0ArrayProperty\0\0\0\0\0\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 30, ArrayProperty (NewInt64LE 0) (NewList []))
        decodeProperty "\14\0\0\0ArrayProperty\0\1\0\0\0\0\0\0\0\1\0\0\0\1\0\0\0\0\13\0\0\0BoolProperty\0\0\0\0\0\0\0\0\0\0\5\0\0\0None\0" `shouldBe` Right ("", 70, ArrayProperty (NewInt64LE 1) (NewList [NewDictionary (Map.singleton (NewPCString "") (BoolProperty (NewInt64LE 0) (NewBoolean False)))]))
    it "can decode booleans" $ do
        decodeProperty "\13\0\0\0BoolProperty\0\0\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 26, BoolProperty (NewInt64LE 0) (NewBoolean False))
        decodeProperty "\13\0\0\0BoolProperty\0\1\0\0\0\0\0\0\0\1" `shouldBe` Right ("", 26, BoolProperty (NewInt64LE 1) (NewBoolean True))
    it "can decode bytes" $ do
        decodeProperty "\13\0\0\0ByteProperty\0\0\0\0\0\0\0\0\0\1\0\0\0\0\1\0\0\0\0" `shouldBe` Right ("", 35, ByteProperty (NewInt64LE 0) (NewPCString "", NewPCString ""))
        decodeProperty "\13\0\0\0ByteProperty\0\1\0\0\0\0\0\0\0\2\0\0\0a\0\2\0\0\0b\0" `shouldBe` Right ("", 37, ByteProperty (NewInt64LE 1) (NewPCString "a", NewPCString "b"))
    it "can decode floats" $ do
        decodeProperty "\14\0\0\0FloatProperty\0\4\0\0\0\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 30, FloatProperty (NewInt64LE 4) (NewFloat32LE 0.0))
        decodeProperty "\14\0\0\0FloatProperty\0\4\0\0\0\0\0\0\0\0\0\128\63" `shouldBe` Right ("", 30, FloatProperty (NewInt64LE 4) (NewFloat32LE 1.0))
    it "can decode integers" $ do
        decodeProperty "\12\0\0\0IntProperty\0\4\0\0\0\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 28, IntProperty (NewInt64LE 4) (NewInt32LE 0))
        decodeProperty "\12\0\0\0IntProperty\0\4\0\0\0\0\0\0\0\1\0\0\0" `shouldBe` Right ("", 28, IntProperty (NewInt64LE 4) (NewInt32LE 1))
    it "can decode names" $ do
        decodeProperty "\13\0\0\0NameProperty\0\0\0\0\0\0\0\0\0\1\0\0\0\0" `shouldBe` Right ("", 30, NameProperty (NewInt64LE 0) (NewPCString ""))
        decodeProperty "\13\0\0\0NameProperty\0\1\0\0\0\0\0\0\0\2\0\0\0a\0" `shouldBe` Right ("", 31, NameProperty (NewInt64LE 1) (NewPCString "a"))
    it "can decode qwords" $ do
        decodeProperty "\14\0\0\0QWordProperty\0\8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 34, QWordProperty (NewInt64LE 8) (NewInt64LE 0))
        decodeProperty "\14\0\0\0QWordProperty\0\8\0\0\0\0\0\0\0\2\0\0\0\0\0\0\0" `shouldBe` Right ("", 34, QWordProperty (NewInt64LE 8) (NewInt64LE 2))
    it "can decode strings" $ do
        decodeProperty "\12\0\0\0StrProperty\0\0\0\0\0\0\0\0\0\1\0\0\0\0" `shouldBe` Right ("", 29, StrProperty (NewInt64LE 0) (NewPCString ""))
        decodeProperty "\12\0\0\0StrProperty\0\1\0\0\0\0\0\0\0\2\0\0\0a\0" `shouldBe` Right ("", 30, StrProperty (NewInt64LE 1) (NewPCString "a"))
    it "can encode arrays" $ do
        Binary.encode (ArrayProperty (NewInt64LE 0) (NewList [])) `shouldBe` "\14\0\0\0ArrayProperty\0\0\0\0\0\0\0\0\0\0\0\0\0"
        Binary.encode (ArrayProperty (NewInt64LE 1) (NewList [NewDictionary (Map.singleton (NewPCString "a") (BoolProperty (NewInt64LE 2) (NewBoolean True)))])) `shouldBe` "\14\0\0\0ArrayProperty\0\1\0\0\0\0\0\0\0\1\0\0\0\2\0\0\0a\0\13\0\0\0BoolProperty\0\2\0\0\0\0\0\0\0\1\5\0\0\0None\0"
    it "can encode booleans" $ do
        Binary.encode (BoolProperty (NewInt64LE 0) (NewBoolean False)) `shouldBe` "\13\0\0\0BoolProperty\0\0\0\0\0\0\0\0\0\0"
        Binary.encode (BoolProperty (NewInt64LE 1) (NewBoolean True)) `shouldBe` "\13\0\0\0BoolProperty\0\1\0\0\0\0\0\0\0\1"
    it "can encode bytes" $ do
        Binary.encode (ByteProperty (NewInt64LE 0) (NewPCString "", NewPCString "")) `shouldBe` "\13\0\0\0ByteProperty\0\0\0\0\0\0\0\0\0\1\0\0\0\0\1\0\0\0\0"
        Binary.encode (ByteProperty (NewInt64LE 1) (NewPCString "a", NewPCString "b")) `shouldBe` "\13\0\0\0ByteProperty\0\1\0\0\0\0\0\0\0\2\0\0\0a\0\2\0\0\0b\0"
    it "can encode floats" $ do
        Binary.encode (FloatProperty (NewInt64LE 4) (NewFloat32LE 0.0)) `shouldBe` "\14\0\0\0FloatProperty\0\4\0\0\0\0\0\0\0\0\0\0\0"
        Binary.encode (FloatProperty (NewInt64LE 4) (NewFloat32LE 1.0)) `shouldBe` "\14\0\0\0FloatProperty\0\4\0\0\0\0\0\0\0\0\0\128\63"
    it "can encode integers" $ do
        Binary.encode (IntProperty (NewInt64LE 4) (NewInt32LE 0)) `shouldBe` "\12\0\0\0IntProperty\0\4\0\0\0\0\0\0\0\0\0\0\0"
        Binary.encode (IntProperty (NewInt64LE 4) (NewInt32LE 1)) `shouldBe` "\12\0\0\0IntProperty\0\4\0\0\0\0\0\0\0\1\0\0\0"
    it "can encode names" $ do
        Binary.encode (NameProperty (NewInt64LE 0) (NewPCString "")) `shouldBe` "\13\0\0\0NameProperty\0\0\0\0\0\0\0\0\0\1\0\0\0\0"
        Binary.encode (NameProperty (NewInt64LE 1) (NewPCString "a")) `shouldBe` "\13\0\0\0NameProperty\0\1\0\0\0\0\0\0\0\2\0\0\0a\0"
    it "can encode qwords" $ do
        Binary.encode (QWordProperty (NewInt64LE 8) (NewInt64LE 0)) `shouldBe` "\14\0\0\0QWordProperty\0\8\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
        Binary.encode (QWordProperty (NewInt64LE 8) (NewInt64LE 2)) `shouldBe` "\14\0\0\0QWordProperty\0\8\0\0\0\0\0\0\0\2\0\0\0\0\0\0\0"
    it "can encode strings" $ do
        Binary.encode (StrProperty (NewInt64LE 0) (NewPCString "")) `shouldBe` "\12\0\0\0StrProperty\0\0\0\0\0\0\0\0\0\1\0\0\0\0"
        Binary.encode (StrProperty (NewInt64LE 1) (NewPCString "a")) `shouldBe` "\12\0\0\0StrProperty\0\1\0\0\0\0\0\0\0\2\0\0\0a\0"
    it "does not raise a runtime error when decoding garbage" $ do
        decodeProperty "\14\0\0\0OtherProperty\0\0\0\0\0\0\0\0\0" `shouldBe` Left ("", 26, "unknown property type \"OtherProperty\"")
        decodeProperty "\14\0\0\0FloatProperty\0\0\0\0\0\0\0\0\0" `shouldBe` Left ("", 26, "unknown FloatProperty size 0")
        decodeProperty "\12\0\0\0IntProperty\0\0\0\0\0\0\0\0\0" `shouldBe` Left ("", 24, "unknown IntProperty size 0")
        decodeProperty "\14\0\0\0QWordProperty\0\0\0\0\0\0\0\0\0" `shouldBe` Left ("", 26, "unknown QWordProperty size 0")

decodeProperty :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Property)
decodeProperty = Binary.decodeOrFail
