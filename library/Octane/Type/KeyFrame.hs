{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.KeyFrame (KeyFrame(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified GHC.Generics as Generics
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Word32 as Word32


-- | A key frame.
data KeyFrame = KeyFrame
    { time :: Float32.Float32
    -- ^ When this key frame occurred.
    , frame :: Word32.Word32
    -- ^ Which frame this key frame corresponds to.
    , position :: Word32.Word32
    -- ^ The bit position of the start of this key frame in the network stream.
    } deriving (Eq, Generics.Generic, Show)

-- | Stored with the fields one after the other in order.
--
-- >>> Binary.decode "\x00\x00\x00\x00\x01\x00\x00\x00\x02\x00\x00\x00" :: KeyFrame
-- KeyFrame {time = 0.0, frame = 0x00000001, position = 0x00000002}
--
-- >>> Binary.encode (KeyFrame { time = 0, frame = 1, position = 2 })
-- "\NUL\NUL\NUL\NUL\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL"
instance Binary.Binary KeyFrame where
    get = KeyFrame
        <$> Binary.get
        <*> Binary.get
        <*> Binary.get

    put keyFrame = do
        keyFrame & time & Binary.put
        keyFrame & frame & Binary.put
        keyFrame & position & Binary.put

instance DeepSeq.NFData KeyFrame where
