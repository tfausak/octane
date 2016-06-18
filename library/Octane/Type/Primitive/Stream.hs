{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Stream (Stream(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as StrictBytes
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Word32 as Word32
import qualified Octane.Utility as Utility


-- | A length-prefixed stream of bits. The length is given in bytes. Each byte
-- is reversed such that @0b01234567@ is actually @0b76543210@.
newtype Stream = Stream
    { unpackStream :: StrictBytes.ByteString
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Stream where
    get = do
        size <- Binary.get
        content <- size & Word32.fromWord32 & Binary.getByteString
        content & StrictBytes.map Utility.reverseBits & Stream & return
    put stream = do
        let content = unpackStream stream
        content & StrictBytes.length & fromIntegral & Word32.Word32 & Binary.put
        content & StrictBytes.map Utility.reverseBits & Binary.putByteString

instance DeepSeq.NFData Stream where

instance Aeson.ToJSON Stream where
    toJSON stream = let
        size = stream & unpackStream & StrictBytes.length
        bytes = if size == 1 then "byte"else "bytes"
        in Aeson.toJSON (unwords ["Stream:", show size, bytes, "..."])
