{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Octane.Type.Stream (Stream(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Utility.Endian as Endian


-- | A length-prefixed stream of bits. The length is given in bytes. Each byte
-- is reversed such that @0b01234567@ is actually @0b76543210@.
newtype Stream = Stream
    { unpack :: LazyBytes.ByteString
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Stream where
    get = do
        size <- Binary.get
        content <- size & Word32.fromWord32 & Binary.getLazyByteString
        content & Endian.reverseBitsInBytes & Stream & return
    put stream = do
        let content = unpack stream
        content & LazyBytes.length & Word32.toWord32 & Binary.put
        content & Endian.reverseBitsInBytes & Binary.putLazyByteString

instance DeepSeq.NFData Stream where
