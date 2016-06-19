{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Vector (Vector(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified GHC.Generics as Generics


data Vector a = Vector
    { x :: a
    , y :: a
    , z :: a
    } deriving (Eq, Generics.Generic, Show)

instance (DeepSeq.NFData a) => DeepSeq.NFData (Vector a) where

instance (Aeson.ToJSON a) => Aeson.ToJSON (Vector a) where
    toJSON vector = Aeson.toJSON
        [ x vector
        , y vector
        , z vector
        ]
