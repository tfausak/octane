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

module Octane.Type.Value.ExplosionValue
  ( ExplosionValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Vector as Vector

data ExplosionValue = ExplosionValue
  { explosionValueActorless :: Boolean.Boolean
  , explosionValueActorId :: Maybe Int32.Int32
  , explosionValuePosition :: Vector.Vector Int
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData ExplosionValue

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

$(OverloadedRecords.overloadedRecord Default.def ''ExplosionValue)
