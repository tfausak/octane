{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.List
  ( List(..)
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords

-- | A list of values.
newtype List a = List
  { listUnpack :: [a]
  } deriving (Eq, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''List)

instance (Show a) =>
         Show (List a) where
  show list = "fromList " ++ show (#unpack list)

-- | Encoded as a JSON array directly.
instance (Aeson.ToJSON a) =>
         Aeson.ToJSON (List a) where
  toJSON list = list & #unpack & Aeson.toJSON
