module Octane.Type.Replication (Replication(..)) where

import Basics

import qualified Data.Map.Strict as Map
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.State as State
import qualified Octane.Type.Value as Value


-- | A replicated actor in a frame.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it
-- requires out-of-band information (the class property map) to decode.
data Replication = Replication
    { replicationActorId :: CompressedWord.CompressedWord
    -- ^ The actor's ID.
    , replicationObjectName :: StrictText
    -- ^ The name of the actor's object.
    , replicationClassName :: StrictText
    -- ^ The name of the actor's class.
    , replicationState :: State.State
    -- ^ Which state this actor's replication is in.
    , replicationInitialization :: Maybe Initialization.Initialization
    -- ^ The optional initialization information for this actor. These only
    -- exist for new actors.
    , replicationProperties :: Map.Map StrictText Value.Value
    -- ^ The property updates associated with this actor's replication.
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''Replication)

instance NFData Replication where
