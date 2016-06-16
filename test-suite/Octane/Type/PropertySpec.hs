{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.PropertySpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map

spec :: Spec
spec = describe "Property" $ do
    describe "Array" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0ArrayProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\0\0\0\0")
                (Right ("", 30, ArrayProperty
                    (Int64 0)
                    (List [])))
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0ArrayProperty\0\
                    \\1\0\0\0\0\0\0\0\1\0\0\0\2\0\0\0a\0\13\0\0\0BoolProperty\0\2\0\0\0\0\0\0\0\1\5\0\0\0None\0")
                (Right ("", 71, ArrayProperty
                    (Int64 1)
                    (List [Dictionary (Map.singleton (Text "a") (BoolProperty (Int64 2) (Boolean True)))])))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (ArrayProperty
                    (Int64 0)
                    (List [])))
                "\
                    \\14\0\0\0ArrayProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\0\0\0\0"
            shouldBe
                (Binary.encode (ArrayProperty
                    (Int64 1)
                    (List [Dictionary (Map.singleton (Text "a") (BoolProperty (Int64 2) (Boolean True)))])))
                "\
                    \\14\0\0\0ArrayProperty\0\
                    \\1\0\0\0\0\0\0\0\1\0\0\0\2\0\0\0a\0\13\0\0\0BoolProperty\0\2\0\0\0\0\0\0\0\1\5\0\0\0None\0"
    describe "Bool" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0BoolProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\0")
                (Right ("", 26, BoolProperty
                    (Int64 0)
                    (Boolean False)))
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0BoolProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\1")
                (Right ("", 26, BoolProperty
                    (Int64 1)
                    (Boolean True)))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (BoolProperty
                    (Int64 0)
                    (Boolean False)))
                "\
                    \\13\0\0\0BoolProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\0"
            shouldBe
                (Binary.encode (BoolProperty
                    (Int64 1)
                    (Boolean True)))
                "\
                    \\13\0\0\0BoolProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\1"
    describe "Byte" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0ByteProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0\1\0\0\0\0")
                (Right ("", 35, ByteProperty
                    (Int64 0)
                    (Text "", Text "")))
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0ByteProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0\2\0\0\0b\0")
                (Right ("", 37, ByteProperty
                    (Int64 1)
                    (Text "a", Text "b")))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (ByteProperty
                    (Int64 0)
                    (Text "", Text "")))
                "\
                    \\13\0\0\0ByteProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0\1\0\0\0\0"
            shouldBe
                (Binary.encode (ByteProperty
                    (Int64 1)
                    (Text "a", Text "b")))
                "\
                    \\13\0\0\0ByteProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0\2\0\0\0b\0"
    describe "Float" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0FloatProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\0\0")
                (Right ("", 30, FloatProperty
                    (Int64 4)
                    (Float32 0.0)))
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0FloatProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\128\63")
                (Right ("", 30, FloatProperty
                    (Int64 4)
                    (Float32 1.0)))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (FloatProperty
                    (Int64 4)
                    (Float32 0.0)))
                "\
                    \\14\0\0\0FloatProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\0\0"
            shouldBe
                (Binary.encode (FloatProperty
                    (Int64 4)
                    (Float32 1.0)))
                "\
                    \\14\0\0\0FloatProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\128\63"
        it "does not raise a runtime error when decoding garbage" $ do
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0FloatProperty\0\
                    \\0\0\0\0\0\0\0\0")
                (Left ("", 26, "unknown FloatProperty size 0"))
    describe "Int" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\12\0\0\0IntProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\0\0")
                (Right ("", 28, IntProperty
                    (Int64 4)
                    (Int32 0)))
            shouldBe
                (decodeProperty "\
                    \\12\0\0\0IntProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\1\0\0\0")
                (Right ("", 28, IntProperty
                    (Int64 4)
                    (Int32 1)))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (IntProperty
                    (Int64 4)
                    (Int32 0)))
                "\
                    \\12\0\0\0IntProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\0\0"
            shouldBe
                (Binary.encode (IntProperty
                    (Int64 4)
                    (Int32 1)))
                "\
                    \\12\0\0\0IntProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\1\0\0\0"
        it "does not raise a runtime error when decoding garbage" $ do
            shouldBe
                (decodeProperty "\
                    \\12\0\0\0IntProperty\0\
                    \\0\0\0\0\0\0\0\0")
                (Left ("", 24, "unknown IntProperty size 0"))
    describe "Name" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0NameProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0")
                (Right ("", 30, NameProperty
                    (Int64 0)
                    (Text "")))
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0NameProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0")
                (Right ("", 31, NameProperty
                    (Int64 1)
                    (Text "a")))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (NameProperty
                    (Int64 0)
                    (Text "")))
                "\
                    \\13\0\0\0NameProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0"
            shouldBe
                (Binary.encode (NameProperty
                    (Int64 1)
                    (Text "a")))
                "\
                    \\13\0\0\0NameProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0"
    describe "QWord" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0QWordProperty\0\
                    \\8\0\0\0\0\0\0\0\
                    \\0\0\0\0\0\0\0\0")
                (Right ("", 34, QWordProperty
                    (Int64 8)
                    (Int64 0)))
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0QWordProperty\0\
                    \\8\0\0\0\0\0\0\0\
                    \\2\0\0\0\0\0\0\0")
                (Right ("", 34, QWordProperty
                    (Int64 8)
                    (Int64 2)))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (QWordProperty
                    (Int64 8)
                    (Int64 0)))
                "\
                    \\14\0\0\0QWordProperty\0\
                    \\8\0\0\0\0\0\0\0\
                    \\0\0\0\0\0\0\0\0"
            shouldBe
                (Binary.encode (QWordProperty
                    (Int64 8)
                    (Int64 2)))
                "\
                    \\14\0\0\0QWordProperty\0\
                    \\8\0\0\0\0\0\0\0\
                    \\2\0\0\0\0\0\0\0"
        it "does not raise a runtime error when decoding garbage" $ do
            shouldBe
                (decodeProperty "\14\0\0\0QWordProperty\0\0\0\0\0\0\0\0\0")
                (Left ("", 26, "unknown QWordProperty size 0"))
    describe "Str" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\12\0\0\0StrProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0")
                (Right ("", 29, StrProperty
                    (Int64 0)
                    (Text "")))
            shouldBe
                (decodeProperty "\
                    \\12\0\0\0StrProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0")
                (Right ("", 30, StrProperty
                    (Int64 1)
                    (Text "a")))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (StrProperty
                    (Int64 0)
                    (Text "")))
                "\
                    \\12\0\0\0StrProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0"
            shouldBe
                (Binary.encode (StrProperty
                    (Int64 1)
                    (Text "a")))
                "\
                    \\12\0\0\0StrProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0"
    it "does not raise a runtime error when decoding garbage" $ do
        shouldBe
            (decodeProperty "\
                \\14\0\0\0OtherProperty\0")
            (Left ("", 18, "unknown property type \"OtherProperty\""))

decodeProperty :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Property)
decodeProperty = Binary.decodeOrFail
