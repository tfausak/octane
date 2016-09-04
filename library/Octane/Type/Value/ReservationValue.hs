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

module Octane.Type.Value.ReservationValue
  ( ReservationValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word8 as Word8

data ReservationValue = ReservationValue
  { reservationValueNumber :: CompressedWord.CompressedWord
  , reservationValueSystemId :: Word8.Word8
  , reservationValueRemoteId :: RemoteId.RemoteId
  , reservationValueLocalId :: Maybe Word8.Word8
  , reservationValuePlayerName :: Maybe Text.Text
  , reservationValueUnknown1 :: Boolean.Boolean
  , reservationValueUnknown2 :: Boolean.Boolean
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ReservationValue)

instance DeepSeq.NFData ReservationValue

instance Aeson.ToJSON ReservationValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("Reservation" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "Number" .= #number x
          , "SystemId" .= #systemId x
          , "RemoteId" .= #remoteId x
          , "LocalId" .= #localId x
          , "Name" .= #playerName x
          , "Unknown1" .= #unknown1 x
          , "Unknown2" .= #unknown2 x
          ]
      ]
