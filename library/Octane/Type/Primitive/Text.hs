{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Text (Text(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Int32 as Int32

-- | A length-prefixed null-terminated string.
newtype Text = Text
    { unpackText :: Text.Text
    } deriving (Eq,Generics.Generic,Ord,Show)

instance Binary.Binary Text where
    get = do
        (Int32.Int32 rawSize) <- Binary.get
        -- In some tiny percentage of replays, this nonsensical string size
        -- shows up. As far as I can tell the next 3 bytes are always null. And
        -- the actual string is "None", which is 5 bytes including the null
        -- terminator.
        --
        -- These annoying replays come from around 2015-10-25 to 2015-11-01.
        size <- if rawSize == 0x05000000
            then do
                bytes <- Binary.getByteString 3
                if BS.all (== 0) bytes
                    then return 5
                    else error
                        ( "read special size "
                        ++ show rawSize
                        ++ " but next 3 bytes were "
                        ++ show bytes
                        ++ " instead of all null"
                        )
            else return rawSize
        string <-
            if size == 0
                then fail ("invalid Text size " ++ show size)
                else if size < 0
                         then do
                             let actualSize = 2 * negate size
                             bytes <-
                                 Binary.getByteString (fromIntegral actualSize)
                             bytes & Encoding.decodeUtf16LE & return
                         else do
                             bytes <- Binary.getByteString (fromIntegral size)
                             bytes & Encoding.decodeLatin1 & return
        string & Text.dropEnd 1 & Text & return
    put string = do
        let cString = string & unpackText & flip Text.snoc '\NUL'
        let size = cString & Text.length & fromIntegral
        if Text.all Char.isLatin1 cString
            then do
                size & Int32.Int32 & Binary.put
                cString & encodeLatin1 & Binary.putByteString
            else do
                size & negate & Int32.Int32 & Binary.put
                cString & Encoding.encodeUtf16LE & Binary.putByteString

instance String.IsString Text where
    fromString string = string & Text.pack & Text

instance DeepSeq.NFData Text

instance Aeson.ToJSON Text where
    toJSON string = string & unpackText & Aeson.toJSON

encodeLatin1 :: Text.Text -> BS.ByteString
encodeLatin1 text = text & Text.unpack & BS8.pack
