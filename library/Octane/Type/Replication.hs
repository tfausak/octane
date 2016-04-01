{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Replication where

import Octane.Internal.Core

data Replication = Replication
    {
    } deriving (Eq, Generic, NFData, Show)
