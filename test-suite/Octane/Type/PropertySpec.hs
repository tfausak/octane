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
    describe "Array" $ do
        it "can be decoded" $ do
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0ArrayProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\0\0\0\0")
                (Right ("", 30, ArrayProperty
                    (Word64LE 0)
                    (List [])))
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0ArrayProperty\0\
                    \\1\0\0\0\0\0\0\0\1\0\0\0\2\0\0\0a\0\13\0\0\0BoolProperty\0\2\0\0\0\0\0\0\0\1\5\0\0\0None\0")
                (Right ("", 71, ArrayProperty
                    (Word64LE 1)
                    (List [Dictionary (Map.singleton (PCString "a") (BoolProperty (Word64LE 2) (Boolean True)))])))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (ArrayProperty
                    (Word64LE 0)
                    (List [])))
                "\
                    \\14\0\0\0ArrayProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\0\0\0\0"
            shouldBe
                (Binary.encode (ArrayProperty
                    (Word64LE 1)
                    (List [Dictionary (Map.singleton (PCString "a") (BoolProperty (Word64LE 2) (Boolean True)))])))
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
                    (Word64LE 0)
                    (Boolean False)))
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0BoolProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\1")
                (Right ("", 26, BoolProperty
                    (Word64LE 1)
                    (Boolean True)))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (BoolProperty
                    (Word64LE 0)
                    (Boolean False)))
                "\
                    \\13\0\0\0BoolProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\0"
            shouldBe
                (Binary.encode (BoolProperty
                    (Word64LE 1)
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
                    (Word64LE 0)
                    (PCString "", PCString "")))
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0ByteProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0\2\0\0\0b\0")
                (Right ("", 37, ByteProperty
                    (Word64LE 1)
                    (PCString "a", PCString "b")))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (ByteProperty
                    (Word64LE 0)
                    (PCString "", PCString "")))
                "\
                    \\13\0\0\0ByteProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0\1\0\0\0\0"
            shouldBe
                (Binary.encode (ByteProperty
                    (Word64LE 1)
                    (PCString "a", PCString "b")))
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
                    (Word64LE 4)
                    (Float32LE 0.0)))
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0FloatProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\128\63")
                (Right ("", 30, FloatProperty
                    (Word64LE 4)
                    (Float32LE 1.0)))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (FloatProperty
                    (Word64LE 4)
                    (Float32LE 0.0)))
                "\
                    \\14\0\0\0FloatProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\0\0"
            shouldBe
                (Binary.encode (FloatProperty
                    (Word64LE 4)
                    (Float32LE 1.0)))
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
                    (Word64LE 4)
                    (Word32LE 0)))
            shouldBe
                (decodeProperty "\
                    \\12\0\0\0IntProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\1\0\0\0")
                (Right ("", 28, IntProperty
                    (Word64LE 4)
                    (Word32LE 1)))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (IntProperty
                    (Word64LE 4)
                    (Word32LE 0)))
                "\
                    \\12\0\0\0IntProperty\0\
                    \\4\0\0\0\0\0\0\0\
                    \\0\0\0\0"
            shouldBe
                (Binary.encode (IntProperty
                    (Word64LE 4)
                    (Word32LE 1)))
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
                    (Word64LE 0)
                    (PCString "")))
            shouldBe
                (decodeProperty "\
                    \\13\0\0\0NameProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0")
                (Right ("", 31, NameProperty
                    (Word64LE 1)
                    (PCString "a")))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (NameProperty
                    (Word64LE 0)
                    (PCString "")))
                "\
                    \\13\0\0\0NameProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0"
            shouldBe
                (Binary.encode (NameProperty
                    (Word64LE 1)
                    (PCString "a")))
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
                    (Word64LE 8)
                    (Word64LE 0)))
            shouldBe
                (decodeProperty "\
                    \\14\0\0\0QWordProperty\0\
                    \\8\0\0\0\0\0\0\0\
                    \\2\0\0\0\0\0\0\0")
                (Right ("", 34, QWordProperty
                    (Word64LE 8)
                    (Word64LE 2)))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (QWordProperty
                    (Word64LE 8)
                    (Word64LE 0)))
                "\
                    \\14\0\0\0QWordProperty\0\
                    \\8\0\0\0\0\0\0\0\
                    \\0\0\0\0\0\0\0\0"
            shouldBe
                (Binary.encode (QWordProperty
                    (Word64LE 8)
                    (Word64LE 2)))
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
                    (Word64LE 0)
                    (PCString "")))
            shouldBe
                (decodeProperty "\
                    \\12\0\0\0StrProperty\0\
                    \\1\0\0\0\0\0\0\0\
                    \\2\0\0\0a\0")
                (Right ("", 30, StrProperty
                    (Word64LE 1)
                    (PCString "a")))
        it "can be encoded" $ do
            shouldBe
                (Binary.encode (StrProperty
                    (Word64LE 0)
                    (PCString "")))
                "\
                    \\12\0\0\0StrProperty\0\
                    \\0\0\0\0\0\0\0\0\
                    \\1\0\0\0\0"
            shouldBe
                (Binary.encode (StrProperty
                    (Word64LE 1)
                    (PCString "a")))
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
