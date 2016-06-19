{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Message (Message(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32

-- | A debugging message. Replays do not have any of these anymore.
data Message = Message
    { frame :: Word32.Word32
    , name :: Text.Text
    , content :: Text.Text
    } deriving (Eq, Generics.Generic, Show)

instance Binary.Binary Message where
    get = Message <$> Binary.get <*> Binary.get <*> Binary.get
    put message = do
        message & frame & Binary.put
        message & name & Binary.put
        message & content & Binary.put

instance DeepSeq.NFData Message where
