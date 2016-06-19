{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replication (Replication(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map.Strict as Map
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.State as State
import qualified Octane.Type.Value as Value


-- TODO
data Replication = Replication
    { actorId :: Word
    , objectName :: StrictText.Text
    , className :: StrictText.Text
    , state :: State.State
    , initialization :: Maybe Initialization.Initialization
    , properties :: Map.Map StrictText.Text Value.Value
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Replication where
