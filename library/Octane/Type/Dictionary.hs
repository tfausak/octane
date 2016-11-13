{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Dictionary
  ( Dictionary(..)
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.Map.Strict as Map
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Octane.Type.Text as Text

-- | A mapping between text and arbitrary values.
newtype Dictionary a = Dictionary
  { dictionaryUnpack :: (Map.Map Text.Text a)
  } deriving (Eq)

$(OverloadedRecords.overloadedRecord Default.def ''Dictionary)

-- | Shown as @fromList [("key","value")]@.
instance (Show a) =>
         Show (Dictionary a) where
  show dictionary = show (#unpack dictionary)

-- | Encoded directly as a JSON object.
instance (Aeson.ToJSON a) =>
         Aeson.ToJSON (Dictionary a) where
  toJSON dictionary = dictionary & #unpack & Map.mapKeys #unpack & Aeson.toJSON
