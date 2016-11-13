{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.PrivateMatchSettingsValue
  ( PrivateMatchSettingsValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word32 as Word32

data PrivateMatchSettingsValue = PrivateMatchSettingsValue
  { privateMatchSettingsValueMutators :: Text.Text
  , privateMatchSettingsValueJoinableBy :: Word32.Word32
  , privateMatchSettingsValueMaxPlayers :: Word32.Word32
  , privateMatchSettingsValueGameName :: Text.Text
  , privateMatchSettingsValuePassword :: Text.Text
  , privateMatchSettingsValueFlag :: Boolean.Boolean
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''PrivateMatchSettingsValue)

instance Aeson.ToJSON PrivateMatchSettingsValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("PrivateMatchSettings" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Mutators" .= #mutators x
          , "JoinableBy" .= #joinableBy x
          , "MaxPlayers" .= #maxPlayers x
          , "Name" .= #gameName x
          , "Password" .= #password x
          , "Unknown" .= #flag x
          ]
      ]
