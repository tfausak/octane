module Octane.Types.Message where

import qualified Data.Binary as Binary
import Octane.Core
import Octane.Types.Int32LE
import Octane.Types.PCString

data Message = NewMessage {
    messageFrame :: Int32LE,
    messageName :: PCString,
    messageContent :: PCString
} deriving (Show)

instance Binary.Binary Message where
    get = do
        frame <- Binary.get
        name <- Binary.get
        content <- Binary.get
        return NewMessage {
            messageFrame = frame,
            messageName = name,
            messageContent = content
        }

    put message = do
        message & messageFrame & Binary.put
        message & messageName & Binary.put
        message & messageContent & Binary.put
