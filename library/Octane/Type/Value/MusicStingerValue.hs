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

module Octane.Type.Value.MusicStingerValue
  ( MusicStingerValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word8 as Word8

data MusicStingerValue = MusicStingerValue
  { musicStingerValueFlag :: Boolean.Boolean
  , musicStingerValueCue :: Word32.Word32
  , musicStingerValueTrigger :: Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData MusicStingerValue

instance Aeson.ToJSON MusicStingerValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("MusicStinger" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          ["Flag" .= #flag x, "Cue" .= #cue x, "Trigger" .= #trigger x]
      ]

$(OverloadedRecords.overloadedRecord Default.def ''MusicStingerValue)
