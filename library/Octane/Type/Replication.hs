{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Replication (Replication(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Map.Strict as Map
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.State as State
import qualified Octane.Type.Value as Value


-- | A replicated actor in a frame.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it
-- requires out-of-band information (the class property map) to decode.
data Replication = Replication
    { actorId :: CompressedWord.CompressedWord
    -- ^ The actor's ID.
    , objectName :: StrictText.Text
    -- ^ The name of the actor's object.
    , className :: StrictText.Text
    -- ^ The name of the actor's class.
    , state :: State.State
    -- ^ Which state this actor's replication is in.
    , initialization :: Maybe Initialization.Initialization
    -- ^ The optional initialization information for this actor. These only
    -- exist for new actors.
    , properties :: Map.Map StrictText.Text Value.Value
    -- ^ The property updates associated with this actor's replication.
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Replication where
