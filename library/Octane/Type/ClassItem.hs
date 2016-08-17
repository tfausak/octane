module Octane.Type.ClassItem (ClassItem(..)) where

import Basics

import qualified Data.Binary as Binary
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


-- | A class (like @Core.Object@) and it's associated ID in the net stream
-- (like @0@).
data ClassItem = ClassItem
    { classItemName :: Text.Text
    -- ^ The class's name.
    , classItemStreamId :: Word32.Word32
    -- ^ The class's ID in the network stream.
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''ClassItem)

-- | Fields are stored one after the other in order.
instance Binary ClassItem where
    get = ClassItem
        <$> Binary.get
        <*> Binary.get

    put classItem = do
        classItem & #name & Binary.put
        classItem & #streamId & Binary.put

instance NFData ClassItem where
