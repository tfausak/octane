{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.CacheItemSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

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
                (Word32LE 0)
                (Word32LE 0)
                (Word32LE 0)
                (List [])))
        shouldBe
            (decodeCacheItem "\
                \\1\0\0\0\
                \\2\0\0\0\
                \\3\0\0\0\
                \\1\0\0\0\
                \\4\0\0\0\5\0\0\0")
            (Right ("", 24, CacheItem
                (Word32LE 1)
                (Word32LE 2)
                (Word32LE 3)
                (List [CacheProperty (Word32LE 4) (Word32LE 5)])))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (CacheItem
                (Word32LE 0)
                (Word32LE 0)
                (Word32LE 0)
                (List [])))
            "\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0\
                \\0\0\0\0"
        shouldBe
            (Binary.encode (CacheItem
                (Word32LE 1)
                (Word32LE 2)
                (Word32LE 3)
                (List [CacheProperty (Word32LE 4) (Word32LE 5)])))
            "\
                \\1\0\0\0\
                \\2\0\0\0\
                \\3\0\0\0\
                \\1\0\0\0\
                \\4\0\0\0\5\0\0\0"

decodeCacheItem :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, CacheItem)
decodeCacheItem = Binary.decodeOrFail
