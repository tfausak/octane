module Octane.Types.ActorMap where

import Octane.Types.Actor (Actor (NewActor), actorName, actorTag)
import Octane.Types.Int32LE (Int32LE (NewInt32LE), getInt32LE)
import Octane.Types.List (List (NewList))
import Octane.Types.PCString (PCString)

import qualified Data.Binary as B
import qualified Data.IntMap as M

newtype ActorMap = NewActorMap
    { getActorMap :: M.IntMap PCString
    } deriving (Show)

instance B.Binary ActorMap where
    get = do
        NewList list <- B.get
        return (NewActorMap (M.fromList (map (\ a -> (getInt32LE (actorTag a), actorName a)) list)))

    put (NewActorMap actorMap) = do
        B.put (NewList (map (\ (tag, name) -> NewActor { actorName = name, actorTag = NewInt32LE tag }) (M.assocs actorMap)))
