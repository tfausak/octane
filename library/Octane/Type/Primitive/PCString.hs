{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.PCString (PCString(..)) where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import Octane.Core
import Octane.Type.Primitive.Word32LE

-- | A length-prefixed null-terminated string.
newtype PCString = PCString Text
    deriving (Eq, Generic, NFData, Ord, Show)

instance Binary PCString where
    get = do
        (Word32LE size) <- get
        string <- if size == 0
            then fail ("invalid PCString size " ++ show size)
            else if size < 0
            then do
                let actualSize = 2 * negate size
                bytes <- Binary.getByteString (fromIntegral actualSize)
                bytes & Encoding.decodeUtf16LE & return
            else do
                bytes <- Binary.getByteString (fromIntegral size)
                bytes & Encoding.decodeLatin1 & return
        string & Text.dropEnd 1 & pack & return

    put string = do
        let cString = string & unpack & flip Text.snoc '\NUL'
        let size = cString & Text.length & fromIntegral
        if Text.all Char.isLatin1 cString
        then do
            size & Word32LE & put
            cString & encodeLatin1 & Binary.putByteString
        else do
            size & negate & Word32LE & put
            cString & Encoding.encodeUtf16LE & Binary.putByteString

instance Newtype PCString

instance ToJSON PCString

encodeLatin1 :: Text -> ByteString
encodeLatin1 text = text & Text.unpack & BS8.pack
