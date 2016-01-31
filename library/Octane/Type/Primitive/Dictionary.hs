{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Dictionary (Dictionary(..)) where

import qualified Data.Map as Map
import Octane.Core
import Octane.Type.Primitive.PCString

newtype Dictionary a = NewDictionary
    { getDictionary :: Map PCString a
    } deriving (Eq, Show)

instance (Binary a) => Binary (Dictionary a) where
    get = do
        element <- getElement
        if Map.null element
        then do
            element & NewDictionary & return
        else do
            NewDictionary elements <- get
            elements & Map.union element & NewDictionary & return

    put (NewDictionary elements) = do
        elements & Map.assocs & mapM_ putElement
        "None" & NewPCString & put

getElement :: (Binary a) => Get (Map PCString a)
getElement = do
    key <- get
    if key == NewPCString "None"
    then do
        return Map.empty
    else do
        value <- get
        return (Map.singleton key value)


putElement :: (Binary a) => (PCString, a) -> Put
putElement (key, value) = do
    key & put
    value & put
