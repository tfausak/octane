{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Frame (Frame(..)) where

import Octane.Internal.Core
import Octane.Type.Replication

data Frame = Frame
    { frameTime :: ByteString -- TODO: This should be a Float.
    , frameDelta :: ByteString -- TODO: This should be a Float.
    , frameReplications :: [Replication]
    } deriving (Eq, Generic, NFData, Show)
