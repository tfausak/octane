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
    , frame :: Word32.Word32
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Mark where
    get = Mark <$> Binary.get <*> Binary.get
    put mark = do
        mark & label & Binary.put
        mark & frame & Binary.put

instance DeepSeq.NFData Mark
