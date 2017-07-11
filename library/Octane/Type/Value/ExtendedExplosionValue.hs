{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.ExtendedExplosionValue
  ( ExtendedExplosionValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Vector as Vector

data ExtendedExplosionValue = ExtendedExplosionValue
  { extendedExplosionValueActorless :: Boolean.Boolean
  , extendedExplosionValueActorId :: Maybe Int32.Int32
  , extendedExplosionValuePosition :: Vector.Vector Int
  , extendedExplosionValueUnknown1 :: Boolean.Boolean
  , extendedExplosionValueUnknown2 :: Int32.Int32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ExtendedExplosionValue)

instance Aeson.ToJSON ExtendedExplosionValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("ExtendedExplosion" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Actorless" .= #actorless x
          , "ActorId" .= #actorId x
          , "Position" .= #position x
          ]
      ]
