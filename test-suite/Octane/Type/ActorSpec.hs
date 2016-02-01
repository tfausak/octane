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
        shouldBe
            (decodeActor "\
                \\1\0\0\0\0\
                \\0\0\0\0\
                \")
            (Right ("", 9, Actor
                (PCString "")
                (Int32LE 0)))
        shouldBe
            (decodeActor "\
                \\2\0\0\0a\0\
                \\2\0\0\0\
                \")
            (Right ("", 10, Actor
                (PCString "a")
                (Int32LE 2)))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Actor
                (PCString "")
                (Int32LE 0)))
            "\
                \\1\0\0\0\0\
                \\0\0\0\0\
                \"
        shouldBe
            (Binary.encode (Actor
                (PCString "a")
                (Int32LE 2)))
            "\
                \\2\0\0\0a\0\
                \\2\0\0\0\
                \"

decodeActor :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Actor)
decodeActor = Binary.decodeOrFail
