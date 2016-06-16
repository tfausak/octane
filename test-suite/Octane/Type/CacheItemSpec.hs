{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.CacheItemSpec (spec) where

import Octane
import Test.Tasty.Hspec

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL

spec :: Spec
spec = describe "CacheItem" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeCacheItem "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0")
            (Right ("", 16, CacheItem
                (Int32 0)
                (Int32 0)
                (Int32 0)
                (List [])))
        shouldBe
            (decodeCacheItem "\
                \\1\0\0\0\
                \\2\0\0\0\
                \\3\0\0\0\
                \\1\0\0\0\
                \\4\0\0\0\5\0\0\0")
            (Right ("", 24, CacheItem
                (Int32 1)
                (Int32 2)
                (Int32 3)
                (List [CacheProperty (Int32 4) (Int32 5)])))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (CacheItem
                (Int32 0)
                (Int32 0)
                (Int32 0)
                (List [])))
            "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0"
        shouldBe
            (Binary.encode (CacheItem
                (Int32 1)
                (Int32 2)
                (Int32 3)
                (List [CacheProperty (Int32 4) (Int32 5)])))
            "\
                \\1\0\0\0\
                \\2\0\0\0\
                \\3\0\0\0\
                \\1\0\0\0\
                \\4\0\0\0\5\0\0\0"

decodeCacheItem :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, CacheItem)
decodeCacheItem = Binary.decodeOrFail
