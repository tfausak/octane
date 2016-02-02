{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Stream (Stream(..)) where

import qualified Data.ByteString as BS
import Octane.Core
import Octane.Type.Primitive.Int32LE

newtype Stream = Stream
    { getStream :: ByteString
    } deriving (Eq, Generic, NFData, Show)

instance Binary Stream where
    get = do
        size <- get
        content <- size & getInt32LE & fromIntegral & getByteString
        content & Stream & return

    put stream = do
        let content = getStream stream
        let size = BS.length content
        size & fromIntegral & Int32LE & put
        content & putByteString
