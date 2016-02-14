{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Replication (Replication(..)) where

import Octane.Core

data Replication = Replication
    {
    } deriving (Eq, Generic, NFData, Show)

instance BinaryBit Replication where
    getBits _ = do
        return Replication
            {
            }

    putBits _ _ = do
        return ()
