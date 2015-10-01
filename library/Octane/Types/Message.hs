module Octane.Types.Message where

import Octane.Types.Int32LE (Int32LE)
import Octane.Types.PCString (PCString)

import qualified Data.Binary as B

data Message = NewMessage
    { messageFrame :: Int32LE
    , messageName :: PCString
    , messageContent :: PCString
    } deriving (Show)

instance B.Binary Message where
    get = NewMessage
        <$> B.get
        <*> B.get
        <*> B.get

    put message = do
        B.put (messageFrame message)
        B.put (messageName message)
        B.put (messageContent message)
