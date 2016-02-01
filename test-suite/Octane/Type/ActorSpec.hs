{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.ActorSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Actor" $ do
    it "can be decoded" $ do
        decodeActor "\1\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 9, NewActor (NewPCString "") (NewInt32LE 0))
        decodeActor "\2\0\0\0a\0\2\0\0\0" `shouldBe` Right ("", 10, NewActor (NewPCString "a") (NewInt32LE 2))
    it "can be encoded" $ do
        Binary.encode (NewActor (NewPCString "") (NewInt32LE 0)) `shouldBe` "\1\0\0\0\0\0\0\0\0"
        Binary.encode (NewActor (NewPCString "a") (NewInt32LE 2)) `shouldBe` "\2\0\0\0a\0\2\0\0\0"

decodeActor :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Actor)
decodeActor = Binary.decodeOrFail
