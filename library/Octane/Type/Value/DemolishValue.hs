{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.DemolishValue
  ( DemolishValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word32 as Word32

data DemolishValue = DemolishValue
  { demolishValueAttackerFlag :: Boolean.Boolean
  , demolishValueAttackerActorId :: Word32.Word32
  , demolishValueVictimFlag :: Boolean.Boolean
  , demolishValueVictimActorId :: Word32.Word32
  , demolishValueAttackerVelocity :: Vector.Vector Int
  , demolishValueVictimVelocity :: Vector.Vector Int
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''DemolishValue)

instance Aeson.ToJSON DemolishValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Demolition" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "AttackerFlag" .= #attackerFlag x
          , "AttackerActorId" .= #attackerActorId x
          , "VictimFlag" .= #victimFlag x
          , "VictimActorId" .= #victimActorId x
          , "AttackerVelocity" .= #attackerVelocity x
          , "VictimVelocity" .= #victimVelocity x
          ]
      ]
