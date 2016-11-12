{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Property
  ( Property(..)
  , module Octane.Type.Property.ArrayProperty
  , module Octane.Type.Property.BoolProperty
  , module Octane.Type.Property.ByteProperty
  , module Octane.Type.Property.FloatProperty
  , module Octane.Type.Property.IntProperty
  , module Octane.Type.Property.NameProperty
  , module Octane.Type.Property.QWordProperty
  , module Octane.Type.Property.StrProperty
  ) where

import Octane.Type.Property.ArrayProperty
import Octane.Type.Property.BoolProperty
import Octane.Type.Property.ByteProperty
import Octane.Type.Property.FloatProperty
import Octane.Type.Property.IntProperty
import Octane.Type.Property.NameProperty
import Octane.Type.Property.QWordProperty
import Octane.Type.Property.StrProperty

import qualified Data.Aeson as Aeson

-- | A metadata property. All properties have a size, but only some actually
-- use it. The value stored in the property can be an array, a boolean, and
-- so on.
data Property
  = PropertyArray (ArrayProperty Property)
  | PropertyBool BoolProperty
  | PropertyByte ByteProperty
  | PropertyFloat FloatProperty
  | PropertyInt IntProperty
  | PropertyName NameProperty
  | PropertyQWord QWordProperty
  | PropertyStr StrProperty
  deriving (Eq, Show)

instance Aeson.ToJSON Property where
  toJSON property =
    case property of
      PropertyArray x -> Aeson.toJSON x
      PropertyBool x -> Aeson.toJSON x
      PropertyByte x -> Aeson.toJSON x
      PropertyFloat x -> Aeson.toJSON x
      PropertyInt x -> Aeson.toJSON x
      PropertyName x -> Aeson.toJSON x
      PropertyQWord x -> Aeson.toJSON x
      PropertyStr x -> Aeson.toJSON x
