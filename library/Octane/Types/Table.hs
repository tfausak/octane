{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Table (Table(..)) where

import qualified Data.Map as Map
import Octane.Core
import Octane.Types.PCString

newtype Table a = NewTable
    { getTable :: Map PCString a
    } deriving (Show)

instance (Binary a) => Binary (Table a) where
    get = do
        row <- getRow
        if Map.null row
        then do
            row & NewTable & return
        else do
            NewTable rows <- get
            rows & Map.union row & NewTable & return

    put (NewTable rows) = do
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
