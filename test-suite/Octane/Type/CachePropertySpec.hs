{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.CachePropertySpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "CacheProperty" $ do
    it "can be decoded" $ do
        decodeCacheProperty "\0\0\0\0\0\0\0\0" `shouldBe` Right ("", 8, CacheProperty (Int32LE 0) (Int32LE 0))
        decodeCacheProperty "\1\0\0\0\2\0\0\0" `shouldBe` Right ("", 8, CacheProperty (Int32LE 1) (Int32LE 2))
    it "can be encoded" $ do
        Binary.encode (CacheProperty (Int32LE 0) (Int32LE 0)) `shouldBe` "\0\0\0\0\0\0\0\0"
        Binary.encode (CacheProperty (Int32LE 1) (Int32LE 2)) `shouldBe` "\1\0\0\0\2\0\0\0"

decodeCacheProperty :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, CacheProperty)
decodeCacheProperty = Binary.decodeOrFail
