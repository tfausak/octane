{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.PCString (PCString(..)) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import Octane.Core
import Octane.Type.Primitive.Int32LE

-- | A length-prefixed null-terminated string.
newtype PCString = PCString
    { getPCString :: Text
    } deriving (Eq, Generic, NFData, Ord, Show)

instance Binary PCString where
    get = do
        (Int32LE size) <- get
        string <- if size == 0
            then fail "invalid size"
            else if size < 0
            then do
                let actualSize = 2 * negate size
                bytes <- getByteString (fromIntegral actualSize)
                bytes & decodeUtf16LE & return
            else do
                bytes <- getByteString (fromIntegral size)
                bytes & decodeLatin1 & return
        string & Text.dropEnd 1 & PCString & return

    put (PCString string) = do
        let cString = Text.snoc string '\NUL'
        let size = cString & Text.length & fromIntegral
        if Text.all isLatin1 cString
        then do
            size & Int32LE & put
            cString & encodeLatin1 & putByteString
        else do
            size & negate & Int32LE & put
            cString & encodeUtf16LE & putByteString

encodeLatin1 :: Text -> ByteString
encodeLatin1 text = text & Text.unpack & BS8.pack
