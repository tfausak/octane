{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.RemoteId
  ( RemoteId(..)
  , module Octane.Type.RemoteId.PlayStationId
  , module Octane.Type.RemoteId.SplitscreenId
  , module Octane.Type.RemoteId.SteamId
  , module Octane.Type.RemoteId.XboxId
  ) where

import Data.Aeson ((.=))
import Octane.Type.RemoteId.PlayStationId
import Octane.Type.RemoteId.SplitscreenId
import Octane.Type.RemoteId.SteamId
import Octane.Type.RemoteId.XboxId

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text

-- | A player's canonical remote ID. This is the best way to uniquely identify
-- players
data RemoteId
  = RemotePlayStationId PlayStationId
  | RemoteSplitscreenId SplitscreenId
  | RemoteSteamId SteamId
  | RemoteXboxId XboxId
  deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RemoteId

-- | Encodes the remote ID as an object with "Type" and "Value" keys.
instance Aeson.ToJSON RemoteId where
  toJSON remoteId =
    case remoteId of
      RemotePlayStationId x ->
        Aeson.object
          ["Type" .= ("PlayStation" :: Text.Text), "Value" .= Aeson.toJSON x]
      RemoteSplitscreenId x ->
        Aeson.object
          ["Type" .= ("Splitscreen" :: Text.Text), "Value" .= Aeson.toJSON x]
      RemoteSteamId x ->
        Aeson.object
          ["Type" .= ("Steam" :: Text.Text), "Value" .= Aeson.toJSON x]
      RemoteXboxId x ->
        Aeson.object
          ["Type" .= ("Xbox" :: Text.Text), "Value" .= Aeson.toJSON x]
