{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Initialization
  ( Initialization(..)
  ) where

import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Int8 as Int8
import qualified Octane.Type.Vector as Vector

-- | Information about a new instance of a class.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it
-- requires out-of-band information (the class name) to decode.
data Initialization = Initialization
  { initializationLocation :: Maybe (Vector.Vector Int)
    -- ^ The instance's initial position.
  , initializationRotation :: Maybe (Vector.Vector Int8.Int8)
    -- ^ The instance's initial rotation.
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Initialization)
