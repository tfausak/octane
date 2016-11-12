{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.ExplosionValue
  ( ExplosionValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Vector as Vector

data ExplosionValue = ExplosionValue
  { explosionValueActorless :: Boolean.Boolean
  , explosionValueActorId :: Maybe Int32.Int32
  , explosionValuePosition :: Vector.Vector Int
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ExplosionValue)

instance Aeson.ToJSON ExplosionValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Explosion" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Actorless" .= #actorless x
          , "ActorId" .= #actorId x
          , "Position" .= #position x
          ]
      ]
