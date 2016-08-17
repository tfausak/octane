module Octane.Type.Message (Message(..)) where

import Basics

import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32

-- | A debugging message. Replays do not have any of these anymore.
data Message = Message
    { messageFrame :: Word32.Word32
    -- ^ The frame this message corresponds to.
    , messageName :: Text.Text
    -- ^ The primary player name.
    , messageContent :: Text.Text
    -- ^ The actual content of the message.
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''Message)

-- | Fields stored in order, one after the other.
instance Binary Message where
    get = Message
        <$> get
        <*> get
        <*> get

    put message = do
        message & #frame & put
        message & #name & put
        message & #content & put

instance NFData Message where
