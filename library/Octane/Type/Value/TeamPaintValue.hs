{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.TeamPaintValue
  ( TeamPaintValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Bimap as Bimap
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Data as Data
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word8 as Word8

getProduct :: Word32.Word32 -> Maybe StrictText.Text
getProduct x = Bimap.lookup (Word32.fromWord32 x) Data.products

data TeamPaintValue = TeamPaintValue
  { teamPaintValueTeam :: Word8.Word8
  , teamPaintValuePrimaryColor :: Word8.Word8
  , teamPaintValueAccentColor :: Word8.Word8
  , teamPaintValuePrimaryFinish :: Word32.Word32
  , teamPaintValueAccentFinish :: Word32.Word32
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''TeamPaintValue)

instance Aeson.ToJSON TeamPaintValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Paint" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Team" .= #team x
          , "PrimaryColor" .= #primaryColor x
          , "AccentColor" .= #accentColor x
          , "PrimaryFinish" .=
            Aeson.object
              [ "Id" .= #primaryFinish x
              , "Name" .= getProduct (#primaryFinish x)
              ]
          , "AccentFinish" .=
            Aeson.object
              ["Id" .= #accentFinish x, "Name" .= getProduct (#accentFinish x)]
          ]
      ]
