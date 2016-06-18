{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.ClassItem (ClassItem(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32


-- | A class (like @Core.Object@) and it's associated ID in the net stream
-- (like 0).
data ClassItem = ClassItem
    { name :: Text.Text
    , streamId :: Word32.Word32
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary ClassItem where
    get = ClassItem <$> Binary.get <*> Binary.get
    put actor = do
        actor & name & Binary.put
        actor & streamId & Binary.put

instance DeepSeq.NFData ClassItem where
