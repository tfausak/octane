{- |
    A boolean stored as a byte.
-}
module Octane.Parser.Types.Boolean where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))

newtype Boolean = NewBoolean {
    getBoolean :: Bool
} deriving (Show)

instance Aeson.ToJSON Boolean where
    toJSON (NewBoolean boolean) = Aeson.toJSON boolean

instance Binary.Binary Boolean where
    get = do
        boolean <- Binary.getWord8
        boolean & fromIntegral & toEnum & NewBoolean & return

    put (NewBoolean boolean) = do
        boolean & fromEnum & fromIntegral & Binary.putWord8
