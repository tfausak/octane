{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.ClubColorsValue
  ( ClubColorsValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Word8 as Word8

data ClubColorsValue = ClubColorsValue
  { clubColorsValueBlueFlag :: Boolean.Boolean
  , clubColorsValueBlueColor :: Word8.Word8
  , clubColorsValueOrangeFlag :: Boolean.Boolean
  , clubColorsValueOrangeColor :: Word8.Word8
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ClubColorsValue)

instance Aeson.ToJSON ClubColorsValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("ClubColors" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "BlueFlag" .= #blueFlag x
          , "BlueColor" .= #blueColor x
          , "OrangeFlag" .= #orangeFlag x
          , "OrangeColor" .= #orangeColor x
          ]
      ]
