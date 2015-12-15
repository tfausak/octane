{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Table where

import qualified Data.Binary as Binary
import qualified Data.Map as Map
import Octane.Core
import Octane.Types.PCString

newtype Table a = NewTable {
    getTable :: Map.Map PCString a
} deriving (Show)

instance (Binary.Binary a) => Binary.Binary (Table a) where
    get = do
        row <- getRow
        if Map.null row
        then do
            row & NewTable & return
        else do
            NewTable rows <- Binary.get
            rows & Map.union row & NewTable & return

    put (NewTable rows) = do
        rows & Map.assocs & mapM_ putRow
        "None" & NewPCString & Binary.put

getRow :: (Binary.Binary a) => Binary.Get (Map.Map PCString a)
getRow = do
    key <- Binary.get
    if key == NewPCString "None"
    then do
        return Map.empty
    else do
        value <- Binary.get
        return (Map.singleton key value)


putRow :: (Binary.Binary a) => (PCString, a) -> Binary.Put
putRow (key, value) = do
    key & Binary.put
    value & Binary.put
