{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Message (Message(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Json as Json
import qualified Octane.Type.Primitive.Text as Text
import qualified Octane.Type.Primitive.Int32 as Int32

-- | A debugging message. Replays do not have any of these anymore.
data Message = Message
    { messageFrame :: !Int32.Int32
    , messageName :: !Text.Text
    , messageContent :: !Text.Text
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Message where
    get = Message <$> Binary.get <*> Binary.get <*> Binary.get
    put message = do
        message & messageFrame & Binary.put
        message & messageName & Binary.put
        message & messageContent & Binary.put

instance DeepSeq.NFData Message

instance Aeson.ToJSON Message where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "Message")
