{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.RemoteId (RemoteId(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64


data RemoteId
    = SteamId Word64.Word64
    | PlayStationId Text.Text
    | SplitscreenId (Maybe Int)
    | XboxId Word64.Word64
    deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON RemoteId where

instance DeepSeq.NFData RemoteId where

instance Aeson.ToJSON RemoteId where
