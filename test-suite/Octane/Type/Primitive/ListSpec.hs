{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.ListSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "List" $ do
    it "can be decoded" $ do
        decodeList "\0\0\0\0" `shouldBe` Right ("", 4, List [] :: List Boolean)
        decodeList "\1\0\0\0\0\0\0\0" `shouldBe` Right ("", 8, List [Int32LE 0])
        decodeList "\2\0\0\0\0\1" `shouldBe` Right ("", 6, List [Boolean False, Boolean True])
    it "can be encoded" $ do
        Binary.encode (List [] :: List Boolean) `shouldBe` "\0\0\0\0"
        Binary.encode (List [Int32LE 0]) `shouldBe` "\1\0\0\0\0\0\0\0"
        Binary.encode (List [Boolean False, Boolean True]) `shouldBe` "\2\0\0\0\0\1"

decodeList :: (Binary.Binary a) => BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, List a)
decodeList = Binary.decodeOrFail
