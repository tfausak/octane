{-# LANGUAGE OverloadedStrings #-}

module Octane.Parser.Types.Message where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Binary as Binary
import Data.Function ((&))
import Octane.Parser.Types.Int32LE
import Octane.Parser.Types.PCString

data Message = NewMessage {
    messageFrame :: Int32LE,
    messageName :: PCString,
    messageContent :: PCString
} deriving (Show)

instance Aeson.ToJSON Message where
    toJSON message = Aeson.object [
        "frame" .= messageFrame message,
        "name" .= messageName message,
        "content" .= messageContent message
        ]

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
