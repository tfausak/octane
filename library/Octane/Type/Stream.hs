{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Stream (Stream(..)) where

import Octane.Core
import Octane.Type.Primitive.Int32LE

data Stream = Stream
    { streamSize :: Int32LE
    , streamContent :: ByteString
    } deriving (Eq, Generic, NFData, Show)

instance Binary Stream where
    get = do
        size <- get
        content <- size & getInt32LE & fromIntegral & getByteString
        return Stream
            { streamSize = size
            , streamContent = content
            }

    put frame = do
        frame & streamSize & put
        frame & streamContent & putByteString
