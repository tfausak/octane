{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.WeldedInfoValue
  ( WeldedInfoValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Int8 as Int8
import qualified Octane.Type.Vector as Vector

data WeldedInfoValue = WeldedInfoValue
  { weldedInfoValueActive :: Boolean.Boolean
  , weldedInfoValueActorId :: Int32.Int32
  , weldedInfoValueOffset :: Vector.Vector Int
  , weldedInfoValueMass :: Float32.Float32
  , weldedInfoValueRotation :: Vector.Vector Int8.Int8
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''WeldedInfoValue)

instance Aeson.ToJSON WeldedInfoValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("WeldedInfo" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Active" .= #active x
          , "ActorId" .= #actorId x
          , "Offset" .= #offset x
          , "Mass" .= #mass x
          , "Rotation" .= #rotation x
          ]
      ]
