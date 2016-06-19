{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.State (State(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics


data State
    = SOpening
    | SExisting
    | SClosing
    deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON State where

instance DeepSeq.NFData State where

instance Aeson.ToJSON State where
