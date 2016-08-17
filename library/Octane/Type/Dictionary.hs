module Octane.Type.Dictionary (Dictionary(..)) where

import Basics

import qualified Data.Binary as Binary
import qualified Data.Map.Strict as Map
import qualified GHC.Exts as Exts
import qualified Octane.Type.Text as Text


-- | A mapping between text and arbitrary values.
newtype Dictionary a = Dictionary
    { dictionaryUnpack :: (StrictMap Text.Text a)
    } deriving (Eq, Generic)

$(overloadedRecord def ''Dictionary)

-- | Elements are stored with the key first, then the value. The dictionary
-- ends when a key is @"None"@.
instance (Binary a) => Binary (Dictionary a) where
    get = do
        element <- getElement
        if Map.null element
        then element & Dictionary & pure
        else do
            Dictionary elements <- get
            elements & Map.union element & Dictionary & pure

    put dictionary = do
        dictionary & #unpack & Map.assocs & mapM_ putElement
        noneKey & put

-- | Allows creating 'Dictionary' values with 'Exts.fromList'. Also allows
-- 'Dictionary' literals with the @OverloadedLists@ extension.
instance Exts.IsList (Dictionary a) where
    type Item (Dictionary a) = (Text.Text, a)

    fromList items = Dictionary (Map.fromList items)

    toList dictionary = Map.toList (#unpack dictionary)

instance (NFData a) => NFData (Dictionary a) where

-- | Shown as @fromList [("key","value")]@.
instance (Show a) => Show (Dictionary a) where
    show dictionary = show (#unpack dictionary)

-- | Encoded directly as a JSON object.
instance (ToJSON a) => ToJSON (Dictionary a) where
    toJSON dictionary = dictionary
        & #unpack
        & Map.mapKeys #unpack
        & toJSON


getElement :: (Binary a) => Binary.Get (StrictMap Text.Text a)
getElement = do
    key <- get
    if key == noneKey
    then pure Map.empty
    else do
        value <- get
        value & Map.singleton key & pure


putElement :: (Binary a) => (Text.Text, a) -> Binary.Put
putElement (key, value) = do
    put key
    put value


noneKey :: Text.Text
noneKey = "None"
