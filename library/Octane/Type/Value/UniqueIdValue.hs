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

module Octane.Type.Value.UniqueIdValue
  ( UniqueIdValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.Word8 as Word8

data UniqueIdValue = UniqueIdValue
  { uniqueIdValueSystemId :: Word8.Word8
  , uniqueIdValueRemoteId :: RemoteId.RemoteId
  , uniqueIdValueLocalId :: Maybe Word8.Word8
  } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData UniqueIdValue

instance Aeson.ToJSON UniqueIdValue where
  toJSON x =
    Aeson.object
      [ "Type" .= ("UniqueId" :: StrictText.Text)
      , "Value" .=
        Aeson.object
          [ "System" .=
            case #systemId x of
              0 -> "Local"
              1 -> "Steam"
              2 -> "PlayStation"
              4 -> "Xbox"
              y -> "Unknown system " ++ show y
          , "Remote" .= #remoteId x
          , "Local" .= #localId x
          ]
      ]

$(OverloadedRecords.overloadedRecord Default.def ''UniqueIdValue)
