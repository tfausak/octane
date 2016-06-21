{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Mark (Mark(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32

-- | A tick mark on the replay. Both goals and saves make tick marks.
data Mark = Mark
    { label :: Text.Text
    -- ^ The description of the tick mark. Typically something like
    -- @"Team0Goal"@ or @"Team1Save"@ or @"User"@.
    , frame :: Word32.Word32
    -- ^ Which frame this tick mark corresponds to.
    } deriving (Eq, Generics.Generic, Show)

-- | Fields are stored one after the other in order.
instance Binary.Binary Mark where
    get = Mark
        <$> Binary.get
        <*> Binary.get

    put mark = do
        mark & label & Binary.put
        mark & frame & Binary.put

instance DeepSeq.NFData Mark
