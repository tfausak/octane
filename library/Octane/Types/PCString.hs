{- |
    A length-prefixed null-terminated string.
-}
module Octane.Types.PCString where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Flow ((|>))
import Octane.Types.Int32LE

newtype PCString = NewPCString {
    getPCString :: Text.Text
} deriving (Eq, Ord, Show)

instance Binary.Binary PCString where
    get = do
        (NewInt32LE size) <- Binary.get
        bytes <- Binary.getByteString (fromIntegral size)
        return NewPCString {
            getPCString = bytes |> Text.decodeLatin1 |> Text.dropEnd 1
        }

    put (NewPCString string) = do
        let bytes = string |> flip Text.snoc '\NUL' |> encodeLatin1
        bytes |> BS.length |> fromIntegral |> NewInt32LE |> Binary.put
        bytes |> Binary.putByteString

encodeLatin1 :: Text.Text -> BS.ByteString
encodeLatin1 text = text |> Text.unpack |> BS8.pack
