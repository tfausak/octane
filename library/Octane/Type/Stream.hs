{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Stream (Stream(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf


-- | A stream of bits.
newtype Stream = Stream
    { streamUnpack :: LazyBytes.ByteString
    } deriving (Eq, Generics.Generic)

$(OverloadedRecords.overloadedRecord Default.def ''Stream)

-- | Prefixed by a length in bytes. Each byte is reversed such that
-- @0b01234567@ is actually @0b76543210@.
--
-- >>> Binary.decode "\x01\x00\x00\x00\x0f" :: Stream
-- Stream {unpack = "1 byte"}
--
-- >>> Binary.encode (Stream "\xf0")
-- "\SOH\NUL\NUL\NUL\SI"
instance Binary.Binary Stream where
    get = do
        size <- Binary.get
        content <- size & Word32.fromWord32 & Binary.getLazyByteString
        content & Endian.reverseBitsInLazyBytes & Stream & pure
    put stream = do
        let content = #unpack stream
        content & LazyBytes.length & Word32.toWord32 & Binary.put
        content & Endian.reverseBitsInLazyBytes & Binary.putLazyByteString

instance DeepSeq.NFData Stream where

-- | Doesn't show the actual bytes to avoid dumping tons of text.
--
-- >>> show (Stream "\x00")
-- "Stream {unpack = \"1 byte\"}"
--
-- >>> show (Stream "\x00\x00")
-- "Stream {unpack = \"2 bytes\"}"
instance Show Stream where
    show stream = do
        let size = stream & #unpack & LazyBytes.length
        let s = if size == 1 then "" else "s"
        Printf.printf "Stream {unpack = \"%d byte%s\"}" size s
