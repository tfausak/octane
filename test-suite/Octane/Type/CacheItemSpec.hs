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
        decodeCacheItem "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 16, NewCacheItem (NewInt32LE 0) (NewInt32LE 0) (NewInt32LE 0) (NewList []))
        decodeCacheItem "\1\0\0\0\2\0\0\0\3\0\0\0\1\0\0\0\4\0\0\0\5\0\0\0" `shouldBe` Right ("", 24, NewCacheItem (NewInt32LE 1) (NewInt32LE 2) (NewInt32LE 3) (NewList [NewCacheProperty (NewInt32LE 4) (NewInt32LE 5)]))
    it "can be encoded" $ do
        Binary.encode (NewCacheItem (NewInt32LE 0) (NewInt32LE 0) (NewInt32LE 0) (NewList [])) `shouldBe` "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
        Binary.encode (NewCacheItem (NewInt32LE 1) (NewInt32LE 2) (NewInt32LE 3) (NewList [NewCacheProperty (NewInt32LE 4) (NewInt32LE 5)])) `shouldBe` "\1\0\0\0\2\0\0\0\3\0\0\0\1\0\0\0\4\0\0\0\5\0\0\0"

decodeCacheItem :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, CacheItem)
decodeCacheItem = Binary.decodeOrFail
