module Octane.Parser.Types.Int32LE where

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.Function ((&))
import qualified Data.Int as Int

newtype Int32LE = NewInt32LE {
    getInt32LE :: Int.Int32
} deriving (Show)

instance Binary.Binary Int32LE where
    get = do
        int <- Binary.getWord32le
        int & fromIntegral & NewInt32LE & return

    put (NewInt32LE int) = do
        int & fromIntegral & Binary.putWord32le
