{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Boolean
  ( Boolean(..)
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords

-- | A boolean value.
newtype Boolean = Boolean
  { booleanUnpack :: Bool
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Boolean)

-- | Encoded directly as a JSON boolean.
instance Aeson.ToJSON Boolean where
  toJSON boolean = boolean & #unpack & Aeson.toJSON
