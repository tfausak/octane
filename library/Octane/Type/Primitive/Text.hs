{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Text (Text(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Char8 as StrictBytes
import qualified Data.Char as Char
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Int32 as Int32
import qualified Octane.Utility as Utility


-- | A length-prefixed null-terminated string.
newtype Text = Text
    { unpackText :: Text.Text
    } deriving (Eq, Generics.Generic, Ord, Show)

instance Binary.Binary Text where
    get = getText Binary.get Binary.getByteString

    put text = putText
        Binary.put
        Binary.putByteString
        id
        text

instance BinaryBit.BinaryBit Text where
    getBits _ = getText (BinaryBit.getBits 32) (BinaryBit.getByteString)

    putBits _ text = putText
        (BinaryBit.putBits 32)
        BinaryBit.putByteString
        Utility.reverseBitsInBytes'
        text

instance String.IsString Text where
    fromString string = string
        & Text.pack
        & Text

instance Aeson.FromJSON Text where
    parseJSON json = case json of
        Aeson.String text -> text & Text & pure
        _ -> Aeson.typeMismatch "Text" json

instance DeepSeq.NFData Text

instance Aeson.ToJSON Text where
    toJSON text = text
        & unpackText
        & Aeson.toJSON


-- | Given functions to get the size and the raw bytes, gets an actual text
-- value. This handles making sense of the size and decoding the bytes.
getText :: (Monad m) => (m Int32.Int32) -> (Int -> m StrictBytes.ByteString) -> m Text
getText getInt getBytes = do
    (Int32.Int32 rawSize) <- getInt
    (size, decode) <-
        -- In some tiny percentage of replays, this nonsensical string size
        -- shows up. As far as I can tell the next 3 bytes are always null. And
        -- the actual string is "None", which is 5 bytes including the null
        -- terminator.
        --
        -- These annoying replays come from around 2015-10-25 to 2015-11-01.
        if rawSize == 0x05000000
        then do
            bytes <- getBytes 3
            if StrictBytes.all (== '\0') bytes
                then pure (5, Encoding.decodeLatin1)
                else fail ("Unexpected Text bytes " ++ show bytes ++ " after size " ++ show rawSize)
        else if rawSize < 0
        then pure (-2 * fromIntegral rawSize, Encoding.decodeUtf16LE)
        else pure (fromIntegral rawSize, Encoding.decodeLatin1)
    bytes <- getBytes size
    let rawText = decode bytes
    case Text.splitAt (Text.length rawText - 1) rawText of
        (text, "") -> text & Text & pure
        (text, "\0") -> text & Text & pure
        _ -> fail ("Unexpected Text value " ++ show rawText)


-- | Given functions to put the size and the raw bytes, puts an actual text
-- value. This handles converting the size and encoding the bytes.
putText :: (Monad m) => (Int32.Int32 -> m ()) -> (StrictBytes.ByteString -> m ()) -> (StrictBytes.ByteString -> StrictBytes.ByteString) -> Text -> m ()
putText putInt putBytes convertBytes text = do
    let fullText = text & unpackText & flip Text.snoc '\NUL'
    let size = fullText & Text.length & fromIntegral
    if Text.all Char.isLatin1 fullText
        then do
            size & Int32.Int32 & putInt
            fullText & encodeLatin1 & convertBytes & putBytes
        else do
            size & negate & Int32.Int32 & putInt
            fullText & Encoding.encodeUtf16LE & convertBytes & putBytes


-- | Encodes text as Latin-1.
encodeLatin1 :: Text.Text -> StrictBytes.ByteString
encodeLatin1 text = text
    & Text.unpack
    & StrictBytes.pack
