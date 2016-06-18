{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Dictionary (Dictionary(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Map.Strict as Map
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Text as Text


-- | A dictionary that maps strings to values.
newtype Dictionary a = Dictionary
    { unpackDictionary :: (Map.Map Text.Text a)
    } deriving (Eq, Generics.Generic, Show)

-- | Reads elements are stored with the key first, then the value. The
-- dictionary ends when a key is @"None"@.
instance (Binary.Binary a) => Binary.Binary (Dictionary a) where
    get = do
        element <- getElement
        if Map.null element
        then element & Dictionary & pure
        else do
            Dictionary elements <- Binary.get
            elements & Map.union element & Dictionary & pure

    put dictionary = do
        dictionary & unpackDictionary & Map.assocs & mapM_ putElement
        noneKey & Binary.put

instance (DeepSeq.NFData a) => DeepSeq.NFData (Dictionary a) where

instance (Aeson.ToJSON a) => Aeson.ToJSON (Dictionary a) where
    toJSON dictionary = dictionary
        & unpackDictionary
        & Map.mapKeys Text.unpackText
        & Aeson.toJSON


-- | Gets a single element. The returned map will either be empty or a
-- singleton.
getElement :: (Binary.Binary a) => Binary.Get (Map.Map Text.Text a)
getElement = do
    key <- Binary.get
    if key == noneKey
    then pure Map.empty
    else do
        value <- Binary.get
        value & Map.singleton key & pure


-- | Puts a single element.
putElement :: (Binary.Binary a) => (Text.Text, a) -> Binary.Put
putElement (key, value) = do
    Binary.put key
    Binary.put value


-- | The sentinel key used to mark the end of the dictionary.
noneKey :: Text.Text
noneKey = "None"
