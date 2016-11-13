{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.RigidBodyStateValue
  ( RigidBodyStateValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Vector as Vector

data RigidBodyStateValue = RigidBodyStateValue
  { rigidBodyStateValueSleeping :: Boolean.Boolean
  , rigidBodyStateValuePosition :: Vector.Vector Int
  , rigidBodyStateValueRotation :: Vector.Vector Float
  , rigidBodyStateValueLinearVelocity :: Maybe (Vector.Vector Int)
  , rigidBodyStateValueAngularVelocity :: Maybe (Vector.Vector Int)
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''RigidBodyStateValue)

instance Aeson.ToJSON RigidBodyStateValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("RigidBodyState" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Sleeping" .= #sleeping x
          , "Position" .= #position x
          , "Rotation" .= #rotation x
          , "LinearVelocity" .= #linearVelocity x
          , "AngularVelocity" .= #angularVelocity x
          ]
      ]
