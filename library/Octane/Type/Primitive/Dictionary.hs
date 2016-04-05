{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Dictionary (Dictionary(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Newtype as Newtype
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified Data.Map as Map
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.PCString as PCString

-- | A dictionary that maps strings to values. The dictionary is terminated by
-- | the key "None".
newtype Dictionary a = Dictionary (Map.Map PCString.PCString a)
    deriving (Eq, Generics.Generic, Show)

instance (Binary.Binary a) => Binary.Binary (Dictionary a) where
    get = do
        element <- getElement
        if Map.null element
        then do
            element & Newtype.pack & return
        else do
            Dictionary elements <- Binary.get
            elements & Map.union element & Newtype.pack & return

    put dictionary = do
        dictionary & Newtype.unpack & Map.assocs & mapM_ putElement
        "None" & PCString.PCString & Binary.put

instance Newtype.Newtype (Dictionary a)

instance (DeepSeq.NFData a) => DeepSeq.NFData (Dictionary a)

instance (Aeson.ToJSON a) => Aeson.ToJSON (Dictionary a) where
    toJSON dictionary = dictionary & Newtype.unpack & Map.mapKeys Newtype.unpack & Aeson.toJSON

getElement :: (Binary.Binary a) => Binary.Get (Map.Map PCString.PCString a)
getElement = do
    key <- Binary.get
    if key == PCString.PCString "None"
    then do
        Map.empty & return
    else do
        value <- Binary.get
        value & Map.singleton key & return

putElement :: (Binary.Binary a) => (PCString.PCString, a) -> Binary.Put
putElement (key, value) = do
    key & Binary.put
    value & Binary.put
