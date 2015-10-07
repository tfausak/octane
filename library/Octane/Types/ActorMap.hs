module Octane.Types.ActorMap where

import qualified Data.Binary as Binary
import qualified Data.IntMap as IntMap
import Flow ((|>))
import Octane.Types.Actor
import Octane.Types.Int32LE
import Octane.Types.List
import Octane.Types.PCString

newtype ActorMap = NewActorMap {
    getActorMap :: IntMap.IntMap PCString
} deriving (Show)

instance Binary.Binary ActorMap where
    get = do
        actors <- Binary.get
        return NewActorMap {
            getActorMap = actors |> getList |> map toTuple |> IntMap.fromList
        }

    put (NewActorMap actorMap) = do
        actorMap |> IntMap.assocs |> map fromTuple |> NewList |> Binary.put

toTuple :: Actor -> (Int, PCString)
toTuple actor = (actor |> actorTag |> getInt32LE, actor |> actorName)

fromTuple :: (Int, PCString) -> Actor
fromTuple (tag, name) = NewActor { actorName = name, actorTag = NewInt32LE tag }
