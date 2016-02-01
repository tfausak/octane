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
                \\0\0\0\0\
                \")
            (Right ("", 78, NewReplay
                (NewInt32LE 0)
                (NewInt32LE 0)
                (NewInt32LE 0)
                (NewInt32LE 0)
                (NewPCString "")
                (NewDictionary Map.empty)
                (NewInt32LE 0)
                (NewInt32LE 0)
                (NewList [])
                (NewList [])
                ""
                (NewList [])
                (NewList [])
                (NewList [])
                (NewList [])
                (NewList [])
                (NewList [])
                (NewList [])))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (NewReplay
                (NewInt32LE 0)
                (NewInt32LE 0)
                (NewInt32LE 0)
                (NewInt32LE 0)
                (NewPCString "")
                (NewDictionary Map.empty)
                (NewInt32LE 0)
                (NewInt32LE 0)
                (NewList [])
                (NewList [])
                ""
                (NewList [])
                (NewList [])
                (NewList [])
                (NewList [])
                (NewList [])
                (NewList [])
                (NewList [])))
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
                \\0\0\0\0\
                \"

decodeReplay :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Replay)
decodeReplay = Binary.decodeOrFail
