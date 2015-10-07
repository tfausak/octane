{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Types.PCString where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Flow ((|>))
import Octane.Types.Int32LE

newtype PCString = NewPCString {
    getPCString :: Text.Text
} deriving (Eq , Ord , String.IsString , Show)

instance Binary.Binary PCString where
    get = do
        (NewInt32LE size) <- Binary.get
        bytes <- Binary.getByteString size
        return NewPCString {
            getPCString = bytes |> Text.decodeLatin1 |> Text.dropEnd 1
        }

    put (NewPCString string) = do
        let bytes = string |> flip Text.snoc '\NUL' |> encodeLatin1
        bytes |> ByteString.length |> NewInt32LE |> Binary.put
        bytes |> Binary.putByteString

encodeLatin1 :: Text.Text -> ByteString.ByteString
encodeLatin1 text = text |> Text.unpack |> ByteString8.pack
