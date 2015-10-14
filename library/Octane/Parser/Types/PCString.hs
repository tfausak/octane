{- |
    A length-prefixed null-terminated string.
-}
module Octane.Parser.Types.PCString where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Flow ((|>))
import Octane.Parser.Types.Int32LE

newtype PCString = NewPCString {
    getPCString :: Text.Text
} deriving (Eq, Ord, Show)

instance Aeson.ToJSON PCString where
    toJSON (NewPCString string) = Aeson.toJSON string

instance Binary.Binary PCString where
    get = do
        (NewInt32LE size) <- Binary.get
        string <- if size < 0
            then do
                let actualSize = 2 * negate size
                bytes <- Binary.getByteString (fromIntegral actualSize)
                bytes |> Text.decodeUtf16LE |> return
            else do
                bytes <- Binary.getByteString (fromIntegral size)
                bytes |> Text.decodeLatin1 |> return
        string |> Text.dropEnd 1 |> NewPCString |> return

    put (NewPCString string) = do
        let cString = Text.snoc string '\NUL'
        let size = cString |> Text.length |> fromIntegral
        if Text.all Char.isLatin1 cString
        then do
            size |> NewInt32LE |> Binary.put
            cString |> encodeLatin1 |> Binary.putByteString
        else do
            size |> negate |> NewInt32LE |> Binary.put
            cString |> Text.encodeUtf16LE |> Binary.putByteString

encodeLatin1 :: Text.Text -> BS.ByteString
encodeLatin1 text = text |> Text.unpack |> BS8.pack
