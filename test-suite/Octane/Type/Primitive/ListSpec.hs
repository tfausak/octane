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
        shouldBe
            (decodeList "\0\0\0\0")
            (Right ("", 4, List [] :: List Boolean))
        shouldBe
            (decodeList "\1\0\0\0\0\0\0\0")
            (Right ("", 8, List [Word32LE 0]))
        shouldBe
            (decodeList "\2\0\0\0\0\1")
            (Right ("", 6, List [Boolean False, Boolean True]))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (List [] :: List Boolean))
            "\0\0\0\0"
        shouldBe
            (Binary.encode (List [Word32LE 0]))
            "\1\0\0\0\0\0\0\0"
        shouldBe
            (Binary.encode (List [Boolean False, Boolean True]))
            "\2\0\0\0\0\1"

decodeList :: (Binary.Binary a) => BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, List a)
decodeList = Binary.decodeOrFail
