{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Dictionary (Dictionary(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Text as Text

-- | A dictionary that maps strings to values. The dictionary is terminated by
-- the key "None".
newtype Dictionary a = Dictionary
    { unpackDictionary :: (Map.Map Text.Text a)
    } deriving (Eq,Generics.Generic,Show)

instance (Binary.Binary a) => Binary.Binary (Dictionary a) where
    get = do
        element <- getElement
        if Map.null element
            then do
                element & Dictionary & return
            else do
                Dictionary elements <- Binary.get
                elements & Map.union element & Dictionary & return
    put dictionary = do
        dictionary & unpackDictionary & Map.assocs & mapM_ putElement
        noneKey & Binary.put

instance (DeepSeq.NFData a) => DeepSeq.NFData (Dictionary a)

instance (Aeson.ToJSON a) => Aeson.ToJSON (Dictionary a) where
    toJSON dictionary =
        dictionary & unpackDictionary & Map.mapKeys Text.unpackText & Aeson.toJSON

getElement
    :: (Binary.Binary a)
    => Binary.Get (Map.Map Text.Text a)
getElement = do
    key <- Binary.get
    if key == noneKey
        then do
            return Map.empty
        else do
            value <- Binary.get
            value & Map.singleton key & return

putElement
    :: (Binary.Binary a)
    => (Text.Text, a) -> Binary.Put
putElement (key,value) = do
    Binary.put key
    Binary.put value

noneKey :: Text.Text
noneKey = "None"
