module Octane.Types.Boolean (Boolean(..)) where

import Octane.Core

newtype Boolean = NewBoolean
    { getBoolean :: Bool
    } deriving (Show)

instance Binary Boolean where
    get = do
        boolean <- getWord8
        boolean & fromIntegral & toEnum & NewBoolean & return

    put (NewBoolean boolean) = do
        boolean & fromEnum & fromIntegral & putWord8
