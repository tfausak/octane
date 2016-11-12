{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.RemoteId.SplitscreenId
  ( SplitscreenId(..)
  ) where

import Data.Aeson ((.=))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText

newtype SplitscreenId = SplitscreenId
  { splitscreenIdUnpack :: Maybe Int
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''SplitscreenId)

instance Aeson.ToJSON SplitscreenId where
  toJSON splitscreenId =
    Aeson.object
      [ "Type" .= ("Splitscreen" :: StrictText.Text)
      , "Value" .= #unpack splitscreenId
      ]
