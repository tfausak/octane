{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Types.Int64LE where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

newtype Int64LE = NewInt64LE
    { unInt64LE :: Int
    } deriving (Eq, Num, Show)

instance B.Binary Int64LE where
    get = do
        word <- B.getWord64le
        return (NewInt64LE (fromIntegral word))

    put (NewInt64LE int) = do
        B.putWord32le (fromIntegral int)
