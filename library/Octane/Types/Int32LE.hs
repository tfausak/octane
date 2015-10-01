module Octane.Types.Int32LE where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

newtype Int32LE = NewInt32LE
    { unInt32LE :: Int
    } deriving (Show)

instance B.Binary Int32LE where
    get = NewInt32LE <$> fmap fromIntegral B.getWord32le

    put (NewInt32LE int) = B.putWord32le (fromIntegral int)
