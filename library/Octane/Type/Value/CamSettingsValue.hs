{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.CamSettingsValue
  ( CamSettingsValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Float32 as Float32

data CamSettingsValue = CamSettingsValue
  { camSettingsValueFov :: Float32.Float32
  , camSettingsValueHeight :: Float32.Float32
  , camSettingsValueAngle :: Float32.Float32
  , camSettingsValueDistance :: Float32.Float32
  , camSettingsValueStiffness :: Float32.Float32
  , camSettingsValueSwivelSpeed :: Float32.Float32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''CamSettingsValue)

instance Aeson.ToJSON CamSettingsValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("CameraSettings" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "FOV" .= #fov x
          , "Height" .= #height x
          , "Angle" .= #angle x
          , "Distance" .= #distance x
          , "Stiffness" .= #stiffness x
          , "SwivelSpeed" .= #swivelSpeed x
          ]
      ]
