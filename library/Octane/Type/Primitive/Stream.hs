{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Stream (Stream(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Int32 as Int32
import qualified Octane.Utility as Utility

-- | A length-prefixed stream of bits. The length is given in bytes. Each byte
-- is reversed such that 0b01234567 is actually 0b76543210.
newtype Stream = Stream
    { unpackStream :: BS.ByteString
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Stream where
    get = do
        Int32.Int32 size <- Binary.get
        content <- size & fromIntegral & Binary.getByteString
        content & BS.map Utility.reverseBits & Stream & return
    put stream = do
        let content = unpackStream stream
        content & BS.length & fromIntegral & Int32.Int32 & Binary.put
        content & BS.map Utility.reverseBits & Binary.putByteString

instance DeepSeq.NFData Stream

instance Aeson.ToJSON Stream where
    toJSON stream =
        let size = stream & unpackStream & BS.length
            bytes =
                if size == 1
                    then "byte"
                    else "bytes"
        in Aeson.toJSON (unwords ["Stream:", show size, bytes, "..."])
