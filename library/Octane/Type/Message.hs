{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Message (Message(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.PCString as PCString
import qualified Octane.Type.Primitive.Word32LE as Word32LE

-- | A debugging message. Replays do not have any of these anymore.
data Message = Message
    { messageFrame :: Word32LE.Word32LE
    , messageName :: PCString.PCString
    , messageContent :: PCString.PCString
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Message where
    get = Message <$> Binary.get <*> Binary.get <*> Binary.get
    put message = do
        message & messageFrame & Binary.put
        message & messageName & Binary.put
        message & messageContent & Binary.put

instance DeepSeq.NFData Message

instance Aeson.ToJSON Message where
    toJSON =
        Aeson.genericToJSON
            Aeson.defaultOptions
            { Aeson.fieldLabelModifier = drop 7
            }
