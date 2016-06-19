{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.State (State(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified GHC.Generics as Generics


data State
    = SOpening
    | SExisting
    | SClosing
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData State where
