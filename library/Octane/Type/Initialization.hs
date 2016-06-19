{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Initialization (Initialization(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified GHC.Generics as Generics
import qualified Octane.Type.Int8 as Int8
import qualified Octane.Type.Vector as Vector


data Initialization = Initialization
    { location :: Maybe (Vector.Vector Int)
    , rotation :: Maybe (Vector.Vector Int8.Int8)
    } deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Initialization where
