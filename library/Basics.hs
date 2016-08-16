module Basics
    ( module Control.DeepSeq
    , module Data.Aeson
    , module Data.Function
    , module Data.Monoid
    , module GHC.Generics
    , module Prelude
    ) where

import Control.DeepSeq (NFData)
import Data.Aeson ((.=))
import Data.Function ((&))
import Data.Monoid ((<>))
import GHC.Generics (Generic)
import Prelude
