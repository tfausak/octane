{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Frame (Frame(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Replication as Replication


data Frame = Frame
    { number :: Word
    , isKeyFrame :: Bool
    , time :: Float32.Float32
    , delta :: Float32.Float32
    , replications :: [Replication.Replication]
    } deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON Frame where

instance DeepSeq.NFData Frame where

instance Aeson.ToJSON Frame where
