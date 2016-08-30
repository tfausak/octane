{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.KeyFrame
  ( KeyFrame(..)
  ) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Word32 as Word32

-- | A key frame.
data KeyFrame = KeyFrame
  { keyFrameTime :: Float32.Float32
    -- ^ When this key frame occurred.
  , keyFrameFrame :: Word32.Word32
    -- ^ Which frame this key frame corresponds to.
  , keyFramePosition :: Word32.Word32
    -- ^ The bit position of the start of this key frame in the network stream.
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''KeyFrame)

-- | Stored with the fields one after the other in order.
instance Binary.Binary KeyFrame where
  get = KeyFrame <$> Binary.get <*> Binary.get <*> Binary.get
  put keyFrame = do
    keyFrame & #time & Binary.put
    keyFrame & #frame & Binary.put
    keyFrame & #position & Binary.put

instance DeepSeq.NFData KeyFrame
