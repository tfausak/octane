module Octane.Type.Stream (Stream(..)) where

import Basics

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Utility.Endian as Endian
import qualified Text.Printf as Printf


-- | A stream of bits.
newtype Stream = Stream
    { streamUnpack :: LazyBytes.ByteString
    } deriving (Eq, Generic)

$(OverloadedRecords.overloadedRecord Default.def ''Stream)

-- | Prefixed by a length in bytes. Each byte is reversed such that
-- @0b01234567@ is actually @0b76543210@.
instance Binary.Binary Stream where
    get = do
        size <- Binary.get
        content <- size & Word32.fromWord32 & Binary.getLazyByteString
        content & Endian.reverseBitsInLazyBytes & Stream & pure
    put stream = do
        let content = #unpack stream
        content & LazyBytes.length & Word32.toWord32 & Binary.put
        content & Endian.reverseBitsInLazyBytes & Binary.putLazyByteString

instance NFData Stream where

-- | Doesn't show the actual bytes to avoid dumping tons of text.
instance Show Stream where
    show stream = do
        let size = stream & #unpack & LazyBytes.length
        let s = if size == 1 then "" else "s"
        Printf.printf "Stream {unpack = \"%d byte%s\"}" size (s :: String)
