{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Types.PCString where

import Octane.Types.Int32LE (Int32LE (NewInt32LE))

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype PCString = NewPCString
    { unPCString :: T.Text
    } deriving (Eq , Ord , S.IsString , Show)

instance B.Binary PCString where
    get = do
        (NewInt32LE size) <- B.get
        bytes <- B.getByteString (fromIntegral size)
        return (NewPCString (T.dropEnd 1 (T.decodeLatin1 bytes)))

    put (NewPCString string) = do
        let bytes = encodeLatin1 (T.snoc string '\NUL')
        B.put (NewInt32LE (BS.length bytes))
        B.putByteString bytes

encodeLatin1 :: T.Text -> BS.ByteString
encodeLatin1 text = BS8.pack (T.unpack text)
