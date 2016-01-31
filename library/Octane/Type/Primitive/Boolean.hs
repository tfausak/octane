module Octane.Type.Primitive.Boolean (Boolean(..)) where

import Octane.Core

newtype Boolean = NewBoolean
    { getBoolean :: Bool
    } deriving (Eq, Show)

instance Binary Boolean where
    get = do
        boolean <- getWord8
        if boolean > 1
        then fail "out of bounds"
        else boolean & fromIntegral & toEnum & NewBoolean & return

    put (NewBoolean boolean) = do
        boolean & fromEnum & fromIntegral & putWord8
