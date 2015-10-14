module Octane.Parser.Types.ActorMap where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.IntMap as IntMap
import Flow ((|>))
import Octane.Parser.Types.Actor
import Octane.Parser.Types.Int32LE
import Octane.Parser.Types.List
import Octane.Parser.Types.PCString

newtype ActorMap = NewActorMap {
    getActorMap :: IntMap.IntMap PCString
} deriving (Show)

instance Aeson.ToJSON ActorMap where
    toJSON (NewActorMap actorMap) = Aeson.toJSON actorMap

instance Binary.Binary ActorMap where
    get = do
        (NewList actors) <- Binary.get
        actors |> map toTuple |> IntMap.fromList |> NewActorMap |> return

    put (NewActorMap actors) = do
        actors |> IntMap.assocs |> map fromTuple |> NewList |> Binary.put

toTuple :: Actor -> (Int, PCString)
toTuple actor =
    (actor |> actorTag |> getInt32LE |> fromIntegral, actor |> actorName)

fromTuple :: (Int, PCString) -> Actor
fromTuple (tag, name) = NewActor {
    actorName = name,
    actorTag = tag |> fromIntegral |> NewInt32LE
}
