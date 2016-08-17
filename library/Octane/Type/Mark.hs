module Octane.Type.Mark (Mark(..)) where

import Basics

import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32

-- | A tick mark on the replay. Both goals and saves make tick marks.
data Mark = Mark
    { markLabel :: Text.Text
    -- ^ The description of the tick mark. Typically something like
    -- @"Team0Goal"@ or @"Team1Save"@ or @"User"@.
    , markFrame :: Word32.Word32
    -- ^ Which frame this tick mark corresponds to.
    } deriving (Eq, Generic, Show)

$(overloadedRecord def ''Mark)

-- | Fields are stored one after the other in order.
instance Binary Mark where
    get = Mark
        <$> get
        <*> get

    put mark = do
        mark & #label & put
        mark & #frame & put

instance NFData Mark
