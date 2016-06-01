module Octane.Json where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

toJsonOptions :: String -> Aeson.Options
toJsonOptions name = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop (length name)
    }
