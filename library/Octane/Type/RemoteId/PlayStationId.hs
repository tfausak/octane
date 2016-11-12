{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.RemoteId.PlayStationId
  ( PlayStationId(..)
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Text as Text
import qualified Text.Printf as Printf

data PlayStationId = PlayStationId
  { playStationIdName :: Text.Text
  , playStationIdUnknown :: LazyBytes.ByteString
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''PlayStationId)

instance Aeson.ToJSON PlayStationId where
  toJSON playStationId =
    Aeson.object
      [ "Type" .= ("PlayStation" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Name" .= #name playStationId
          , "Unknown" .=
            (playStationId & #unknown & LazyBytes.unpack &
             concatMap (Printf.printf "%02x") &
             ("0x" ++) &
             StrictText.pack)
          ]
      ]
