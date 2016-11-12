{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.LoadoutsOnlineValue
  ( LoadoutsOnlineValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Value.LoadoutOnlineValue
       as LoadoutOnlineValue

data LoadoutsOnlineValue = LoadoutsOnlineValue
  { loadoutsOnlineValueLoadout1 :: LoadoutOnlineValue.LoadoutOnlineValue
  , loadoutsOnlineValueLoadout2 :: LoadoutOnlineValue.LoadoutOnlineValue
  , loadoutsOnlineValueUnknown1 :: Boolean.Boolean
  , loadoutsOnlineValueUnknown2 :: Boolean.Boolean
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''LoadoutsOnlineValue)

instance Aeson.ToJSON LoadoutsOnlineValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("LoadoutsOnline" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Loadout1" .= #loadout1 x
          , "Loadout2" .= #loadout2 x
          , "Unknown1" .= #unknown1 x
          , "Unknown2" .= #unknown2 x
          ]
      ]
