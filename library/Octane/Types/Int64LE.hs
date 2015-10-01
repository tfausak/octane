{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Types.Int64LE where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

newtype Int64LE = NewInt64LE
    { unInt64LE :: Int
    } deriving (Eq, Num, Show)

instance B.Binary Int64LE where
    get = NewInt64LE <$> fmap fromIntegral B.getWord64le

    put (NewInt64LE int) = B.putWord64le (fromIntegral int)
