{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.State
  ( State(..)
  , isOpening
  , isExisting
  , isClosing
  ) where

import qualified Control.DeepSeq as DeepSeq
import qualified GHC.Generics as Generics

-- | The state of an actor in a replication.
data State
  = Opening
    -- ^ This is a new actor that we have not seen before.
  | Existing
    -- ^ We have seen this actor before.
  | Closing
    -- ^ This actor is going away.
  deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData State

isOpening :: State -> Bool
isOpening state = state == Opening

isExisting :: State -> Bool
isExisting state = state == Existing

isClosing :: State -> Bool
isClosing state = state == Closing
