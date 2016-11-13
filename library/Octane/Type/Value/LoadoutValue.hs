{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.LoadoutValue
  ( LoadoutValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Data as Data
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word8 as Word8

getProduct :: Word32.Word32 -> Maybe StrictText.Text
getProduct x = Bimap.lookup (Word32.fromWord32 x) Data.products

data LoadoutValue = LoadoutValue
  { loadoutValueVersion :: Word8.Word8
  , loadoutValueBody :: Word32.Word32
  , loadoutValueDecal :: Word32.Word32
  , loadoutValueWheels :: Word32.Word32
  , loadoutValueRocketTrail :: Word32.Word32
  , loadoutValueAntenna :: Word32.Word32
  , loadoutValueTopper :: Word32.Word32
  , loadoutValueUnknown1 :: Word32.Word32
  , loadoutValueUnknown2 :: Maybe Word32.Word32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''LoadoutValue)

instance Aeson.ToJSON LoadoutValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Loadout" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Version" .= #version x
          , "Body" .=
            Aeson.object ["Id" .= #body x, "Name" .= getProduct (#body x)]
          , "Decal" .=
            Aeson.object ["Id" .= #decal x, "Name" .= getProduct (#decal x)]
          , "Wheels" .=
            Aeson.object ["Id" .= #wheels x, "Name" .= getProduct (#wheels x)]
          , "RocketTrail" .=
            Aeson.object
              ["Id" .= #rocketTrail x, "Name" .= getProduct (#rocketTrail x)]
          , "Antenna" .=
            Aeson.object ["Id" .= #antenna x, "Name" .= getProduct (#antenna x)]
          , "Topper" .=
            Aeson.object ["Id" .= #topper x, "Name" .= getProduct (#topper x)]
          , "Unknown1" .= #unknown1 x
          , "Unknown2" .= #unknown2 x
          ]
      ]
