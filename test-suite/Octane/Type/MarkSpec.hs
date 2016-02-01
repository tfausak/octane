{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.MarkSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Mark" $ do
    it "can be decoded" $ do
        decodeMark "\1\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 9, Mark (PCString "") (Int32LE 0))
        decodeMark "\2\0\0\0a\0\1\0\0\0" `shouldBe` Right ("", 10, Mark (PCString "a") (Int32LE 1))
    it "can be encoded" $ do
        Binary.encode (Mark (PCString "") (Int32LE 0)) `shouldBe` "\1\0\0\0\0\0\0\0\0"
        Binary.encode (Mark (PCString "a") (Int32LE 1)) `shouldBe` "\2\0\0\0a\0\1\0\0\0"

decodeMark :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Mark)
decodeMark = Binary.decodeOrFail
