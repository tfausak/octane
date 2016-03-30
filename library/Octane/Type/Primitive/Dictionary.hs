{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Dictionary (Dictionary(..)) where

import qualified Data.Map as Map
import Octane.Internal.Core
import Octane.Type.Primitive.PCString

-- | A dictionary that maps strings to values. The dictionary is terminated by
-- | the key "None".
newtype Dictionary a = Dictionary (Map PCString a)
    deriving (Eq, Generic, NFData, Show)

instance (Binary a) => Binary (Dictionary a) where
    get = do
        element <- getElement
        if Map.null element
        then do
            element & pack & return
        else do
            Dictionary elements <- get
            elements & Map.union element & pack & return

    put dictionary = do
        dictionary & unpack & Map.assocs & mapM_ putElement
        "None" & PCString & put

instance Newtype (Dictionary a)

instance (ToJSON a) => ToJSON (Dictionary a) where
    toJSON dictionary = dictionary & unpack & Map.mapKeys unpack & toJSON

getElement :: (Binary a) => Get (Map PCString a)
getElement = do
    key <- get
    if key == PCString "None"
    then do
        Map.empty & return
    else do
        value <- get
        value & Map.singleton key & return

putElement :: (Binary a) => (PCString, a) -> Put
putElement (key, value) = do
    key & put
    value & put
