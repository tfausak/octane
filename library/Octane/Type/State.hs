module Octane.Type.State (State(..)) where

import Basics



-- | The state of an actor in a replication.
data State
    = SOpening
    -- ^ This is a new actor that we have not seen before.
    | SExisting
    -- ^ We have seen this actor before.
    | SClosing
    -- ^ This actor is going away.
    deriving (Eq, Generic, Show)

instance NFData State where
