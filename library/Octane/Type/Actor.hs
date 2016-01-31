module Octane.Type.Actor (Actor(..)) where

import Octane.Core
import Octane.Type.PCString
import Octane.Type.Primitive.Int32LE

data Actor = NewActor
    { actorName :: PCString
    , actorTag :: Int32LE
    } deriving (Show)

instance Binary Actor where
    get = do
        name <- get
        tag <- get
        return NewActor
            { actorName = name
            , actorTag = tag
            }

    put actor = do
        actor & actorName & put
        actor & actorTag & put
