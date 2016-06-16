{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.CachePropertySpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "CacheProperty" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeCacheProperty "\
                \\0\0\0\0\
                \\0\0\0\0")
            (Right ("", 8, CacheProperty
                (Int32 0)
                (Int32 0)))
        shouldBe
            (decodeCacheProperty "\
                \\1\0\0\0\
                \\2\0\0\0")
            (Right ("", 8, CacheProperty
                (Int32 1)
                (Int32 2)))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (CacheProperty
                (Int32 0)
                (Int32 0)))
            "\
                \\0\0\0\0\
                \\0\0\0\0"
        shouldBe
            (Binary.encode (CacheProperty
                (Int32 1)
                (Int32 2)))
            "\
                \\1\0\0\0\
                \\2\0\0\0"

decodeCacheProperty :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, CacheProperty)
decodeCacheProperty = Binary.decodeOrFail
