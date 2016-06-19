{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.RemoteId (RemoteId(..)) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64


-- | A player's canonical remote ID. This is the best way to uniquely identify
-- players.
data RemoteId
    = SteamId Word64.Word64
    -- ^ A Steam ID.
    | PlayStationId Text.Text
    -- ^ A PlayStation Network ID.
    | SplitscreenId (Maybe Int)
    -- ^ A local splitscreen ID.
    | XboxId Word64.Word64
    -- ^ An Xbox Live ID.
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RemoteId where

instance Aeson.ToJSON RemoteId where
    toJSON remoteId = case remoteId of
        PlayStationId x -> Aeson.object
            [ "Type" .= ("PlayStation" :: Text.Text)
            , "Value" .= x
            ]
        SplitscreenId x -> Aeson.object
            [ "Type" .= ("Splitscreen" :: Text.Text)
            , "Value" .= x
            ]
        SteamId x -> Aeson.object
            [ "Type" .= ("Steam" :: Text.Text)
            , "Value" .= x
            ]
        XboxId x -> Aeson.object
            [ "Type" .= ("Xbox" :: Text.Text)
            , "Value" .= x
            ]
