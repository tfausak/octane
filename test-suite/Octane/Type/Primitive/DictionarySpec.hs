{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.DictionarySpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Dictionary" $ do
    it "can be decoded" $ do
        decodeDictionary "\5\0\0\0None\0" `shouldBe` Right ("", 9, NewDictionary Map.empty :: Dictionary Boolean)
        decodeDictionary "\6\0\0\0Hello\0\6\0\0\0World\0\5\0\0\0None\0" `shouldBe` Right ("", 29, NewDictionary (Map.singleton (NewPCString "Hello") (NewPCString "World")))
        decodeDictionary "\7\0\0\0falsey\0\0\7\0\0\0truthy\0\1\5\0\0\0None\0" `shouldBe` Right ("", 33, NewDictionary (Map.fromList [(NewPCString "truthy", NewBoolean True), (NewPCString "falsey", NewBoolean False)]))
    it "can be encoded" $ do
        Binary.encode (NewDictionary Map.empty :: Dictionary Boolean) `shouldBe` "\5\0\0\0None\0"
        Binary.encode (NewDictionary (Map.singleton (NewPCString "Hello") (NewPCString "World"))) `shouldBe` "\6\0\0\0Hello\0\6\0\0\0World\0\5\0\0\0None\0"
        Binary.encode (NewDictionary (Map.fromList [(NewPCString "truthy", NewBoolean True), (NewPCString "falsey", NewBoolean False)])) `shouldBe` "\7\0\0\0falsey\0\0\7\0\0\0truthy\0\1\5\0\0\0None\0"

decodeDictionary :: (Binary.Binary a) => BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Dictionary a)
decodeDictionary = Binary.decodeOrFail
