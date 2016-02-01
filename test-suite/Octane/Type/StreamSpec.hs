{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.StreamSpec (spec) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.ByteString.Lazy as BSL
import Octane
import Test.Tasty.Hspec

spec :: Spec
spec = describe "Stream" $ do
    it "can be decoded" $ do
        shouldBe
            (decodeStream "\
                \\0\0\0\0\
                \")
            (Right ("", 4, Stream
                (Int32LE 0)
                ""))
    it "can be encoded" $ do
        shouldBe
            (Binary.encode (Stream
                (Int32LE 0)
                ""))
            "\
                \\0\0\0\0\
                \"

decodeStream :: BSL.ByteString -> Either (BSL.ByteString, Binary.ByteOffset, String) (BSL.ByteString, Binary.ByteOffset, Stream)
decodeStream = Binary.decodeOrFail
