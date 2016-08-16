{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Text (Text(..), encodeLatin1) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Char8 as StrictBytes
import qualified Data.Char as Char
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.String as String
import qualified Data.Text as StrictText
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Generics as Generics
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Utility.Endian as Endian

-- $setup
-- >>> import qualified Data.Binary.Get as Binary
-- >>> import qualified Data.Binary.Put as Binary


-- | A thin wrapper around 'StrictText.Text'.
newtype Text = Text
    { textUnpack :: StrictText.Text
    } deriving (Eq, Generics.Generic, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Text)

-- | Text is both length-prefixed and null-terminated.
--
-- >>> Binary.decode "\x02\x00\x00\x00\x4b\x00" :: Text
-- "K"
--
-- >>> Binary.encode ("K" :: Text)
-- "\STX\NUL\NUL\NULK\NUL"
instance Binary.Binary Text where
    get = getText
        Binary.get
        Binary.getByteString
        id

    put text = putText
        Binary.put
        Binary.putByteString
        id
        text

-- | Both length-prefixed and null-terminated. The bits in each byte are
-- reversed.
--
-- >>> Binary.runGet (BinaryBit.runBitGet (BinaryBit.getBits 0)) "\x40\x00\x00\x00\xd2\x00" :: Text
-- "K"
--
-- >>> Binary.runPut (BinaryBit.runBitPut (BinaryBit.putBits 0 ("K" :: Text)))
-- "@\NUL\NUL\NUL\210\NUL"
instance BinaryBit.BinaryBit Text where
    getBits _ = getText
        (BinaryBit.getBits 32)
        BinaryBit.getByteString
        Endian.reverseBitsInStrictBytes

    putBits _ text = putText
        (BinaryBit.putBits 32)
        BinaryBit.putByteString
        Endian.reverseBitsInStrictBytes
        text

-- | Allows you to write 'Text' as string literals with @OverloadedStrings@.
-- Also allows using the 'String.fromString' helper function.
--
-- >>> "K" :: Text
-- "K"
instance String.IsString Text where
    fromString string = Text (StrictText.pack string)

instance DeepSeq.NFData Text where

-- | Shown as a string literal, like @"this"@.
--
-- >>> show ("K" :: Text)
-- "\"K\""
instance Show Text where
    show text = show (#unpack text)

-- | Encoded directly as a JSON string.
--
-- >>> Aeson.encode ("K" :: Text)
-- "\"K\""
instance Aeson.ToJSON Text where
    toJSON text = text
        & #unpack
        & Aeson.toJSON


getText
    :: (Monad m)
    => (m Int32.Int32)
    -> (Int -> m StrictBytes.ByteString)
    -> (StrictBytes.ByteString -> StrictBytes.ByteString)
    -> m Text
getText getInt getBytes convertBytes = do
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
    let rawText = bytes & convertBytes & decode
    case StrictText.splitAt (StrictText.length rawText - 1) rawText of
        (text, "") -> text & Text & pure
        (text, "\0") -> text & Text & pure
        _ -> fail ("Unexpected Text value " ++ show rawText)


putText
    :: (Monad m)
    => (Int32.Int32 -> m ())
    -> (StrictBytes.ByteString -> m ())
    -> (StrictBytes.ByteString -> StrictBytes.ByteString)
    -> Text
    -> m ()
putText putInt putBytes convertBytes text = do
    let fullText = text & #unpack & flip StrictText.snoc '\NUL'
    let size = fullText & StrictText.length & fromIntegral
    if StrictText.all Char.isLatin1 fullText
    then do
        size & Int32.Int32 & putInt
        fullText & encodeLatin1 & convertBytes & putBytes
    else do
        size & negate & Int32.Int32 & putInt
        fullText & Encoding.encodeUtf16LE & convertBytes & putBytes


-- | Encodes text as Latin-1. Note that this isn't really safe if the text has
-- characters that can't be encoded in Latin-1.
--
-- >>> encodeLatin1 "A"
-- "A"
encodeLatin1 :: StrictText.Text -> StrictBytes.ByteString
encodeLatin1 text = text
    & StrictText.unpack
    & StrictBytes.pack
