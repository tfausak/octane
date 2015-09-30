module Octane.Types.Int32LE where

import qualified Data.Binary as B
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

newtype Int32LE = NewInt32LE
    { unInt32LE :: Int
    } deriving (Show)

instance B.Binary Int32LE where
    get = do
        word <- B.getWord32le
        return (NewInt32LE (fromIntegral word))

    put (NewInt32LE int) = do
        B.putWord32le (fromIntegral int)
