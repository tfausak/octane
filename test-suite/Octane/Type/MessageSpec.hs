{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.MessageSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Message" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeMessage "\
                \\0\0\0\0\
                \\1\0\0\0\0\
                \\1\0\0\0\0\
                \")
            (Right ("", 14, Message
                (Int32LE 0)
                (PCString "")
                (PCString "")))
        shouldBe
            (decodeMessage "\
                \\1\0\0\0\
                \\2\0\0\0a\0\
                \\2\0\0\0b\0\
                \")
            (Right ("", 16, Message
                (Int32LE 1)
                (PCString "a")
                (PCString "b")))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Message (Int32LE 0) (PCString "") (PCString "")))
            "\
                \\0\0\0\0\
                \\1\0\0\0\0\
                \\1\0\0\0\0\
                \"
        shouldBe
            (Binary.encode (Message (Int32LE 1) (PCString "a") (PCString "b")))
            "\
                \\1\0\0\0\
                \\2\0\0\0a\0\
                \\2\0\0\0b\0\
                \"

decodeMessage :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Message)
decodeMessage = Binary.decodeOrFail
