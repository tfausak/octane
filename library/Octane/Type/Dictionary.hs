{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Dictionary (Dictionary(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Map.Strict as Map
import qualified GHC.Exts as Exts
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text


-- | A dictionary that maps text to values.
newtype Dictionary a = Dictionary
    { unpack :: (Map.Map Text.Text a)
    } deriving (Eq, Generics.Generic)

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
        dictionary & unpack & Map.assocs & mapM_ putElement
        noneKey & Binary.put

instance Exts.IsList (Dictionary a) where
    type Item (Dictionary a) = (Text.Text, a)

    fromList items = Dictionary (Map.fromList items)

    toList dictionary = Map.toList (unpack dictionary)

instance (DeepSeq.NFData a) => DeepSeq.NFData (Dictionary a) where

instance (Show a) => Show (Dictionary a) where
    show dictionary = show (unpack dictionary)

instance (Aeson.ToJSON a) => Aeson.ToJSON (Dictionary a) where
    toJSON dictionary = dictionary
        & unpack
        & Map.mapKeys Text.unpack
        & Aeson.toJSON


getElement :: (Binary.Binary a) => Binary.Get (Map.Map Text.Text a)
getElement = do
    key <- Binary.get
    if key == noneKey
    then pure Map.empty
    else do
        value <- Binary.get
        value & Map.singleton key & pure


putElement :: (Binary.Binary a) => (Text.Text, a) -> Binary.Put
putElement (key, value) = do
    Binary.put key
    Binary.put value


noneKey :: Text.Text
noneKey = "None"
