module Octane.Type.Message (Message(..)) where

import Octane.Core
import Octane.Type.PCString
import Octane.Type.Primitive.Int32LE

data Message = NewMessage
    { messageFrame :: Int32LE
    , messageName :: PCString
    , messageContent :: PCString
    } deriving (Show)

instance Binary Message where
    get = do
        frame <- get
        name <- get
        content <- get
        return NewMessage
            { messageFrame = frame
            , messageName = name
            , messageContent = content
            }

    put message = do
        message & messageFrame & put
        message & messageName & put
        message & messageContent & put
