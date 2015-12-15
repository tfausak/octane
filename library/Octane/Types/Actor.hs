module Octane.Types.Actor where

import Octane.Core
import Octane.Types.Int32LE
import Octane.Types.PCString

data Actor = NewActor {
    actorName :: PCString,
    actorTag :: Int32LE
} deriving (Show)

instance Binary Actor where
    get = do
        name <- get
        tag <- get
        return NewActor {
            actorName = name,
            actorTag = tag
        }

    put actor = do
        actor & actorName & put
        actor & actorTag & put
