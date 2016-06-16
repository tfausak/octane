{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.MarkSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Mark" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeMark "\
                \\1\0\0\0\0\
                \\0\0\0\0")
            (Right ("", 9, Mark
                (PCString "")
                (Int32 0)))
        shouldBe
            (decodeMark "\
                \\2\0\0\0a\0\
                \\1\0\0\0")
            (Right ("", 10, Mark
                (PCString "a")
                (Int32 1)))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Mark
                (PCString "")
                (Int32 0)))
            "\
                \\1\0\0\0\0\
                \\0\0\0\0"
        shouldBe
            (Binary.encode (Mark
                (PCString "a")
                (Int32 1)))
            "\
                \\2\0\0\0a\0\
                \\1\0\0\0"

decodeMark :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Mark)
decodeMark = Binary.decodeOrFail
