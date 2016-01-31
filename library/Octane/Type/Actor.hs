module Octane.Type.Actor (Actor(..)) where

import Octane.Core
import Octane.Type.Int32LE
import Octane.Type.PCString

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
