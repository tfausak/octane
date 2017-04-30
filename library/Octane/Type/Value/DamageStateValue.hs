{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.DamageStateValue
  ( DamageStateValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word8 as Word8

data DamageStateValue = DamageStateValue
  { damageStateValueUnknown1 :: Word8.Word8
  , damageStateValueUnknown2 :: Boolean.Boolean
  , damageStateValueUnknown3 :: Int32.Int32
  , damageStateValueUnknown4 :: Vector.Vector Int
  , damageStateValueUnknown5 :: Boolean.Boolean
  , damageStateValueUnknown6 :: Boolean.Boolean
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''DamageStateValue)

instance Aeson.ToJSON DamageStateValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("DamageState" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "unknown1" .= damageStateValueUnknown1 x
          , "unknown2" .= damageStateValueUnknown2 x
          , "unknown3" .= damageStateValueUnknown3 x
          , "unknown4" .= damageStateValueUnknown4 x
          , "unknown5" .= damageStateValueUnknown5 x
          , "unknown6" .= damageStateValueUnknown6 x
          ]
      ]
