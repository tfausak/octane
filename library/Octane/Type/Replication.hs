{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Replication (Replication(..)) where

import Octane.Internal.Core

data Replication = Replication
    {
    } deriving (Eq, Generic, NFData, Show)
