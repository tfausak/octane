{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.PickupValue
  ( PickupValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Word32 as Word32

data PickupValue = PickupValue
  { pickupValueHasInstigator :: Boolean.Boolean
  , pickupValueInstigatorId :: Maybe Word32.Word32
  , pickupValuePickedUp :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''PickupValue)

instance DeepSeq.NFData PickupValue

instance Aeson.ToJSON PickupValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Pickup" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "HasInstigator" .= #hasInstigator x
          , "InstigatorId" .= #instigatorId x
          , "PickedUp" .= #pickedUp x
          ]
      ]
