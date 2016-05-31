{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.Boolean (Boolean(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics

-- | A boolean value, stored in the first bit of a byte.
newtype Boolean = Boolean
    { unpackBoolean :: Bool
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Boolean where
    get = do
        boolean <- Binary.getWord8
        if boolean > 1
            then fail ("invalid Boolean value " ++ show boolean)
            else boolean & fromIntegral & toEnum & Newtype.pack & return
    put boolean = do
        boolean & Newtype.unpack & fromEnum & fromIntegral & Binary.putWord8

instance Newtype.Newtype Boolean

instance DeepSeq.NFData Boolean

instance Aeson.ToJSON Boolean
