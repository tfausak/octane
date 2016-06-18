{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.MessageSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "Message" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeMessage "\
                \\0\0\0\0\
                \\1\0\0\0\0\
                \\1\0\0\0\0")
            (Right ("", 14, Message
                (Word32 0)
                (Text "")
                (Text "")))
        shouldBe
            (decodeMessage "\
                \\1\0\0\0\
                \\2\0\0\0a\0\
                \\2\0\0\0b\0")
            (Right ("", 16, Message
                (Word32 1)
                (Text "a")
                (Text "b")))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Message (Word32 0) (Text "") (Text "")))
            "\
                \\0\0\0\0\
                \\1\0\0\0\0\
                \\1\0\0\0\0"
        shouldBe
            (Binary.encode (Message (Word32 1) (Text "a") (Text "b")))
            "\
                \\1\0\0\0\
                \\2\0\0\0a\0\
                \\2\0\0\0b\0"

decodeMessage :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Message)
decodeMessage = Binary.decodeOrFail
