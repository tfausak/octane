{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.Boolean (Boolean(..)) where

import Octane.Internal.Core

import qualified Data.Binary as Binary

-- | A boolean value, stored in the first bit of a byte.
newtype Boolean = Boolean Bool
    deriving (Eq, Generic, NFData, Show)

instance Binary Boolean where
    get = do
        boolean <- Binary.getWord8
        if boolean > 1
        then fail ("invalid Boolean value " ++ show boolean)
        else boolean & fromIntegral & toEnum & pack & return

    put boolean = do
        boolean & unpack & fromEnum & fromIntegral & Binary.putWord8

instance Newtype Boolean

instance ToJSON Boolean
