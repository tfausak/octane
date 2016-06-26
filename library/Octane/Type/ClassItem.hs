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
    -- ^ The class's name.
    , streamId :: Word32.Word32
    -- ^ The class's ID in the network stream.
    } deriving (Eq, Generics.Generic, Show)

-- | Fields are stored one after the other in order.
--
-- >>> Binary.decode "\x02\x00\x00\x00\x4b\x00\x01\x00\x00\x00" :: ClassItem
-- ClassItem {name = "K", streamId = 0x00000001}
--
-- >>> Binary.encode (ClassItem "K" 1)
-- "\STX\NUL\NUL\NULK\NUL\SOH\NUL\NUL\NUL"
instance Binary.Binary ClassItem where
    get = ClassItem
        <$> Binary.get
        <*> Binary.get

    put classItem = do
        classItem & name & Binary.put
        classItem & streamId & Binary.put

instance DeepSeq.NFData ClassItem where
