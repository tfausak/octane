{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.ReplaySpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map

spec :: Spec
spec = describe "Replay" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeReplay "\
                \\x16\x00\x00\x00\
                \\xc5\x30\xa7\xe2\
                \\0\0\0\0\
                \\0\0\0\0\
                \\1\0\0\0\0\
                \\5\0\0\0None\0\
                \\x28\x00\x00\x00\
                \\xf5\x9a\x89\x13\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0")
            (Right ("", 78, Replay
                (Word32 22)
                (Word32 3802607813)
                (Word32 0)
                (Word32 0)
                (Text "")
                (Dictionary Map.empty)
                (Word32 40)
                (Word32 327785205)
                (List [])
                (List [])
                (Stream "")
                (List [])
                (List [])
                (List [])
                (List [])
                (List [])
                (List [])
                (List [])))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Replay
                (Word32 0)
                (Word32 0)
                (Word32 0)
                (Word32 0)
                (Text "")
                (Dictionary Map.empty)
                (Word32 0)
                (Word32 0)
                (List [])
                (List [])
                (Stream "")
                (List [])
                (List [])
                (List [])
                (List [])
                (List [])
                (List [])
                (List [])))
            "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\1\0\0\0\0\
                \\5\0\0\0None\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0"

decodeReplay :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Replay)
decodeReplay = Binary.decodeOrFail
