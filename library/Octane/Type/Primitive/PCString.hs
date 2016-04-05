{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.PCString (PCString(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Char as Char
import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Word32LE as Word32LE

-- | A length-prefixed null-terminated string.
newtype PCString =
    PCString Text.Text
    deriving (Eq,Generics.Generic,Ord,Show)

instance Binary.Binary PCString where
    get = do
        (Word32LE.Word32LE size) <- Binary.get
        string <- 
            if size == 0
                then fail ("invalid PCString size " ++ show size)
                else if size < 0
                         then do
                             let actualSize = 2 * negate size
                             bytes <- 
                                 Binary.getByteString (fromIntegral actualSize)
                             bytes & Encoding.decodeUtf16LE & return
                         else do
                             bytes <- Binary.getByteString (fromIntegral size)
                             bytes & Encoding.decodeLatin1 & return
        string & Text.dropEnd 1 & Newtype.pack & return
    put string = do
        let cString = string & Newtype.unpack & flip Text.snoc '\NUL'
        let size = cString & Text.length & fromIntegral
        if Text.all Char.isLatin1 cString
            then do
                size & Word32LE.Word32LE & Binary.put
                cString & encodeLatin1 & Binary.putByteString
            else do
                size & negate & Word32LE.Word32LE & Binary.put
                cString & Encoding.encodeUtf16LE & Binary.putByteString

instance Newtype.Newtype PCString

instance DeepSeq.NFData PCString

instance Aeson.ToJSON PCString

encodeLatin1 :: Text.Text -> BS.ByteString
encodeLatin1 text = text & Text.unpack & BS8.pack
