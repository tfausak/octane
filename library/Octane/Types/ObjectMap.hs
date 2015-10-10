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
        objects |> zip [0 ..] |> IntMap.fromList |> NewObjectMap |> return

    put (NewObjectMap objects) = do
        objects |> IntMap.elems |> NewList |> Binary.put
