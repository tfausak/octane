{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Dictionary (Dictionary(..)) where

import qualified Data.Map as Map
import Octane.Core
import Octane.Type.Primitive.PCString

newtype Dictionary a = Dictionary
    { getDictionary :: Map PCString a
    } deriving (Eq, Generic, NFData, Show)

instance (Binary a) => Binary (Dictionary a) where
    get = do
        element <- getElement
        if Map.null element
        then do
            element & Dictionary & return
        else do
            Dictionary elements <- get
            elements & Map.union element & Dictionary & return

    put (Dictionary elements) = do
        elements & Map.assocs & mapM_ putElement
        "None" & PCString & put

getElement :: (Binary a) => Get (Map PCString a)
getElement = do
    key <- get
    if key == PCString "None"
    then do
        return Map.empty
    else do
        value <- get
        return (Map.singleton key value)


putElement :: (Binary a) => (PCString, a) -> Put
putElement (key, value) = do
    key & put
    value & put
