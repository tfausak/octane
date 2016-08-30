{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Boolean
  ( Boolean(..)
  ) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics

-- | A boolean value.
newtype Boolean = Boolean
  { booleanUnpack :: Bool
  } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Boolean)

-- | Stored in the last bit of a byte. Decoding will fail if the byte is
-- anything other than @0b00000000@ or @0b00000001@.
instance Binary.Binary Boolean where
  get = do
    value <- Binary.getWord8
    case value of
      0 -> pure (Boolean False)
      1 -> pure (Boolean True)
      _ -> fail ("Unexpected Boolean value " ++ show value)
  put boolean = boolean & #unpack & fromEnum & fromIntegral & Binary.putWord8

-- | Stored as a bit.
instance BinaryBit.BinaryBit Boolean where
  getBits _ = do
    value <- BinaryBit.getBool
    value & Boolean & pure
  putBits _ boolean = boolean & #unpack & BinaryBit.putBool

instance DeepSeq.NFData Boolean

-- | Encoded directly as a JSON boolean.
instance Aeson.ToJSON Boolean where
  toJSON boolean = boolean & #unpack & Aeson.toJSON
