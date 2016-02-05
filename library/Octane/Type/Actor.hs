{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Actor (Actor(..)) where

import Octane.Core
import Octane.Type.Primitive.PCString
import Octane.Type.Primitive.Word32LE

data Actor = Actor
    { actorName :: PCString
    , actorTag :: Word32LE
    } deriving (Eq, Generic, NFData, Show)

instance Binary Actor where
    get = do
        name <- get
        tag <- get
        return Actor
            { actorName = name
            , actorTag = tag
            }

    put actor = do
        actor & actorName & put
        actor & actorTag & put
