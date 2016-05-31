{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Dictionary (Dictionary(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.PCString as PCString

-- | A dictionary that maps strings to values. The dictionary is terminated by
-- the key "None".
newtype Dictionary a = Dictionary
    { unpackDictionary :: (Map.Map PCString.PCString a)
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

instance Newtype.Newtype (Dictionary a)

instance (DeepSeq.NFData a) => DeepSeq.NFData (Dictionary a)

instance (Aeson.ToJSON a) => Aeson.ToJSON (Dictionary a) where
    toJSON dictionary =
        dictionary & unpackDictionary & Map.mapKeys PCString.unpackPCString & Aeson.toJSON

getElement
    :: (Binary.Binary a)
    => Binary.Get (Map.Map PCString.PCString a)
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
    => (PCString.PCString, a) -> Binary.Put
putElement (key,value) = do
    Binary.put key
    Binary.put value

noneKey :: PCString.PCString
noneKey = "None"
