{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.ReplaySpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as Map
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Replay" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeReplay "\
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
                \\0\0\0\0")
            (Right ("", 78, Replay
                (Int32LE 0)
                (Int32LE 0)
                (Int32LE 0)
                (Int32LE 0)
                (PCString "")
                (Dictionary Map.empty)
                (Int32LE 0)
                (Int32LE 0)
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
                (Int32LE 0)
                (Int32LE 0)
                (Int32LE 0)
                (Int32LE 0)
                (PCString "")
                (Dictionary Map.empty)
                (Int32LE 0)
                (Int32LE 0)
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
