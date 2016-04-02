{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Replication (Replication(..)) where

import Octane.Internal.Core

data Replication = Replication
    { replicationActorId :: ByteString -- TODO: This should be an Int.
    , replicationChannelOpen :: Bool
    } deriving (Eq, Generic, NFData, Show)
