{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.RemoteId (RemoteId(..)) where

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
    -- TODO
    | XboxId Word64.Word64
    -- ^ An Xbox Live ID.
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData RemoteId where

-- TODO: Better encoding.
instance Aeson.ToJSON RemoteId where
