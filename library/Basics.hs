module Basics
    ( module Basics
    , module Control.DeepSeq
    , module Data.Aeson
    , module Data.Binary
    , module Data.Binary.Bits
    , module Data.Binary.Bits.Get
    , module Data.Binary.Bits.Put
    , module Data.Default.Class
    , module Data.Function
    , module Data.Monoid
    , module Data.OverloadedRecords.TH
    , module GHC.Generics
    , module Prelude
    , module Text.Printf
    ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON, toJSON, (.=))
import Data.Binary (Binary, get, put)
import Data.Binary.Bits (BinaryBit)
import Data.Binary.Bits.Get (BitGet)
import Data.Binary.Bits.Put (BitPut)
import Data.Default.Class (def)
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.OverloadedRecords.TH (overloadedRecord)
import GHC.Generics (Generic)
import Prelude
import Text.Printf (printf)

import qualified Data.Bimap
import qualified Data.ByteString
import qualified Data.ByteString.Lazy
import qualified Data.Map.Strict
import qualified Data.Set
import qualified Data.Text


type Bimap = Data.Bimap.Bimap

type LazyBytes = Data.ByteString.Lazy.ByteString
type StrictBytes = Data.ByteString.ByteString

type StrictMap = Data.Map.Strict.Map

type Set = Data.Set.Set

type StrictText = Data.Text.Text
