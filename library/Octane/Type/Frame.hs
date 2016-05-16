{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Frame (Frame(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified GHC.Generics as Generics
import qualified Octane.Type.Replication as Replication

-- | A frame in the net stream. Each frame has the time since the beginning of
-- | the match, the time since the last frame, and a list of replications.
data Frame = Frame
    { frameTime :: Float
    , frameDelta :: Float
    , frameReplications :: [Replication.Replication]
    } deriving (Eq,Generics.Generic,Show)

instance DeepSeq.NFData Frame
