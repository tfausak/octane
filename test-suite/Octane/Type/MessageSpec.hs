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
        decodeMessage "\0\0\0\0\1\0\0\0\0\1\0\0\0\0" `shouldBe` Right ("", 14, NewMessage (NewInt32LE 0) (NewPCString "") (NewPCString ""))
        decodeMessage "\1\0\0\0\2\0\0\0a\0\2\0\0\0b\0" `shouldBe` Right ("", 16, NewMessage (NewInt32LE 1) (NewPCString "a") (NewPCString "b"))
    it "can be encoded" $ do
        Binary.encode (NewMessage (NewInt32LE 0) (NewPCString "") (NewPCString "")) `shouldBe` "\0\0\0\0\1\0\0\0\0\1\0\0\0\0"
        Binary.encode (NewMessage (NewInt32LE 1) (NewPCString "a") (NewPCString "b")) `shouldBe` "\1\0\0\0\2\0\0\0a\0\2\0\0\0b\0"

decodeMessage :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Message)
decodeMessage = Binary.decodeOrFail
