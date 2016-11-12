{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.LoadoutsValue
  ( LoadoutsValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Value.LoadoutValue as LoadoutValue

data LoadoutsValue = LoadoutsValue
  { loadoutsValueLoadout1 :: LoadoutValue.LoadoutValue -- ^ blue
  , loadoutsValueLoadout2 :: LoadoutValue.LoadoutValue -- ^ orange
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''LoadoutsValue)

instance Aeson.ToJSON LoadoutsValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Loadouts" :: StrictText.Text)
      , "Value" .=
        Aeson.object ["Loadout1" .= #loadout1 x, "Loadout2" .= #loadout2 x]
      ]
