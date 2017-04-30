{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.AppliedDamageValue
  ( AppliedDamageValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word8 as Word8

data AppliedDamageValue = AppliedDamageValue
  { appliedDamageValueUnknown1 :: Word8.Word8
  , appliedDamageValueLocation :: Vector.Vector Int
  , appliedDamageValueUnknown3 :: Int32.Int32
  , appliedDamageValueUnknown4 :: Int32.Int32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''AppliedDamageValue)

instance Aeson.ToJSON AppliedDamageValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("AppliedDamage" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "unknown1" .= appliedDamageValueUnknown1 x
          , "location" .= appliedDamageValueLocation x
          , "unknown3" .= appliedDamageValueUnknown3 x
          , "unknown4" .= appliedDamageValueUnknown4 x
          ]
      ]
