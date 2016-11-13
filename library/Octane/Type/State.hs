module Octane.Type.State
  ( State(..)
  , isOpening
  , isExisting
  , isClosing
  ) where

-- | The state of an actor in a replication.
data State
  = Opening
    -- ^ This is a new actor that we have not seen before.
  | Existing
    -- ^ We have seen this actor before.
  | Closing
    -- ^ This actor is going away.
  deriving (Eq, Show)

isOpening :: State -> Bool
isOpening state = state == Opening

isExisting :: State -> Bool
isExisting state = state == Existing

isClosing :: State -> Bool
isClosing state = state == Closing
