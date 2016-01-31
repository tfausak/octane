{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Primitive.Dictionary (Dictionary(..)) where

import qualified Data.Map as Map
import Octane.Core
import Octane.Type.Primitive.PCString

newtype Dictionary a = NewDictionary
    { getDictionary :: Map PCString a
    } deriving (Show)

instance (Binary a) => Binary (Dictionary a) where
    get = do
        row <- getRow
        if Map.null row
        then do
            row & NewDictionary & return
        else do
            NewDictionary rows <- get
            rows & Map.union row & NewDictionary & return

    put (NewDictionary rows) = do
        rows & Map.assocs & mapM_ putRow
        "None" & NewPCString & put

getRow :: (Binary a) => Get (Map PCString a)
getRow = do
    key <- get
    if key == NewPCString "None"
    then do
        return Map.empty
    else do
        value <- get
        return (Map.singleton key value)


putRow :: (Binary a) => (PCString, a) -> Put
putRow (key, value) = do
    key & put
    value & put
