{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Message (Message(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
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
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Message)

-- | Fields stored in order, one after the other.
--
-- >>> Binary.decode "\x01\x00\x00\x00\x02\x00\x00\x00\x41\x00\x02\x00\x00\x00\x42\x00" :: Message
-- Message {messageFrame = 0x00000001, messageName = "A", messageContent = "B"}
--
-- >>> Binary.encode (Message 1 "A" "B")
-- "\SOH\NUL\NUL\NUL\STX\NUL\NUL\NULA\NUL\STX\NUL\NUL\NULB\NUL"
instance Binary.Binary Message where
    get = Message
        <$> Binary.get
        <*> Binary.get
        <*> Binary.get

    put message = do
        message & #frame & Binary.put
        message & #name & Binary.put
        message & #content & Binary.put

instance DeepSeq.NFData Message where
