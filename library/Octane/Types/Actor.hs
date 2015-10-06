module Octane.Types.Actor where

import qualified Data.Binary as Binary
import Flow ((|>))
import Octane.Types.Int32LE
import Octane.Types.PCString

data Actor = NewActor {
    actorName :: PCString,
    actorTag :: Int32LE
} deriving (Show)

instance Binary.Binary Actor where
    get = do
        name <- Binary.get
        tag <- Binary.get
        return NewActor {
            actorName = name,
            actorTag = tag
        }

    put actor = do
        actor |> actorName |> Binary.put
        actor |> actorTag |> Binary.put
