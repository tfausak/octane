{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Table where

import qualified Data.Binary as Binary
import qualified Data.Map as Map
import Flow ((|>))
import Octane.Types.PCString (PCString)

newtype Table a = NewTable {
    getTable :: Map.Map PCString a
} deriving (Show)

instance (Binary.Binary a) => Binary.Binary (Table a) where
    get = do
        key <- Binary.get
        if key == "None"
        then do
            return NewTable {
                getTable = Map.empty
            }
        else do
            value <- Binary.get
            NewTable table <- Binary.get
            return NewTable {
                getTable = table |> Map.union (Map.singleton key value)
            }

    put (NewTable table) = do
        table |> Map.assocs |> mapM_ putRow
        ("None" :: PCString) |> Binary.put

putRow :: (Binary.Binary a, Binary.Binary b) => (a, b) -> Binary.Put
putRow (key, value) = do
    key |> Binary.put
    value |> Binary.put
