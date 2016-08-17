module Basics
    ( module Basics
    , module Control.DeepSeq
    , module Data.Aeson
    , module Data.Binary
    , module Data.Default.Class
    , module Data.Function
    , module Data.Monoid
    , module Data.OverloadedRecords.TH
    , module GHC.Generics
    , module Prelude
    ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, toJSON, (.=))
import Data.Binary (Binary)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.OverloadedRecords.TH (overloadedRecord)
import GHC.Generics (Generic)
import Prelude

import qualified Data.ByteString
import qualified Data.ByteString.Lazy


type LazyBytes = Data.ByteString.Lazy.ByteString
type StrictBytes = Data.ByteString.ByteString
