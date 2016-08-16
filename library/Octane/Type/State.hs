module Octane.Type.State (State(..)) where

import Basics

import qualified Control.DeepSeq as DeepSeq
import qualified GHC.Generics as Generics


-- | The state of an actor in a replication.
data State
    = SOpening
    -- ^ This is a new actor that we have not seen before.
    | SExisting
    -- ^ We have seen this actor before.
    | SClosing
    -- ^ This actor is going away.
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData State where
