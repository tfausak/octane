{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Message (Message(..)) where

import Octane.Core
import Octane.Type.Primitive.PCString
import Octane.Type.Primitive.Word32LE

-- | A debugging message. Replays do not have any of these anymore.
data Message = Message
    { messageFrame :: Word32LE
    , messageName :: PCString
    , messageContent :: PCString
    } deriving (Eq, Generic, NFData, Show)

instance Binary Message where
    get = do
        frame <- get
        name <- get
        content <- get
        return Message
            { messageFrame = frame
            , messageName = name
            , messageContent = content
            }

    put message = do
        message & messageFrame & put
        message & messageName & put
        message & messageContent & put
