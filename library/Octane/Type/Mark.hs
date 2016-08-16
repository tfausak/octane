{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Mark (Mark(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32

-- | A tick mark on the replay. Both goals and saves make tick marks.
data Mark = Mark
    { markLabel :: Text.Text
    -- ^ The description of the tick mark. Typically something like
    -- @"Team0Goal"@ or @"Team1Save"@ or @"User"@.
    , markFrame :: Word32.Word32
    -- ^ Which frame this tick mark corresponds to.
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Mark)

-- | Fields are stored one after the other in order.
--
-- >>> Binary.decode "\x02\x00\x00\x00\x4b\x00\x01\x00\x00\x00" :: Mark
-- Mark {markLabel = "K", markFrame = 0x00000001}
--
-- >>> Binary.encode (Mark "K" 1)
-- "\STX\NUL\NUL\NULK\NUL\SOH\NUL\NUL\NUL"
instance Binary.Binary Mark where
    get = Mark
        <$> Binary.get
        <*> Binary.get

    put mark = do
        mark & #label & Binary.put
        mark & #frame & Binary.put

instance DeepSeq.NFData Mark
