module Octane.Types.ObjectMap where

import qualified Data.Binary as Binary
import qualified Data.IntMap as IntMap
import Flow ((|>))
import Octane.Types.List
import Octane.Types.PCString

newtype ObjectMap = NewObjectMap {
    getObjectMap :: IntMap.IntMap PCString
} deriving (Show)

instance Binary.Binary ObjectMap where
    get = do
        NewList objects <- Binary.get
        return NewObjectMap {
            getObjectMap = objects |> zip [0 ..] |> IntMap.fromDistinctAscList
        }

    put (NewObjectMap objectMap) = do
        objectMap |> IntMap.elems |> NewList |> Binary.put
