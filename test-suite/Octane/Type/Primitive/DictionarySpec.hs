{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.DictionarySpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map

spec :: Spec
spec = describe "Dictionary" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeDictionary "\5\0\0\0None\0")
            (Right ("", 9, Dictionary Map.empty :: Dictionary Boolean))
        shouldBe
            (decodeDictionary "\6\0\0\0Hello\0\6\0\0\0World\0\5\0\0\0None\0")
            (Right ("", 29, Dictionary (Map.singleton (PCString "Hello") (PCString "World"))))
        shouldBe
            (decodeDictionary "\7\0\0\0falsey\0\0\7\0\0\0truthy\0\1\5\0\0\0None\0")
            (Right ("", 33, Dictionary (Map.fromList [(PCString "truthy", Boolean True), (PCString "falsey", Boolean False)])))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Dictionary Map.empty :: Dictionary Boolean))
            "\5\0\0\0None\0"
        shouldBe
            (Binary.encode (Dictionary (Map.singleton (PCString "Hello") (PCString "World"))))
            "\6\0\0\0Hello\0\6\0\0\0World\0\5\0\0\0None\0"
        shouldBe
            (Binary.encode (Dictionary (Map.fromList [(PCString "truthy", Boolean True), (PCString "falsey", Boolean False)])))
            "\7\0\0\0falsey\0\0\7\0\0\0truthy\0\1\5\0\0\0None\0"

decodeDictionary :: (Binary.Binary a) => BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Dictionary a)
decodeDictionary = Binary.decodeOrFail
