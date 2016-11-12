{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Value.LoadoutOnlineValue
  ( LoadoutOnlineValue(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Word32 as Word32

newtype LoadoutOnlineValue = LoadoutOnlineValue
  { loadoutOnlineValueUnpack :: [[(Word32.Word32, CompressedWord.CompressedWord)]]
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''LoadoutOnlineValue)

instance Aeson.ToJSON LoadoutOnlineValue where
  toJSON x =
    Aeson.object
      ["Type" .= ("OnlineLoadout" :: StrictText.Text), "Value" .= #unpack x]
