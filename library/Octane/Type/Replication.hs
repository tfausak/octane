{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Replication (Replication(..)) where

import Octane.Internal.Core

data Replication = Replication
    { replicationActorId :: ByteString
    , replicationIsOpen :: Bool
    , replicationIsNew :: Maybe Bool
    } deriving (Eq, Generic, NFData, Show)
