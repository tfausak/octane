{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.CompressedWord
  ( CompressedWord(..)
  , fromCompressedWord
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords

-- | A compressed, unsigned integer. When serialized, the least significant bit
-- is first. Bits are serialized until the next bit would be greater than the
-- limit, or the number of bits necessary to reach the limit has been reached,
-- whichever comes first.
data CompressedWord = CompressedWord
  { compressedWordLimit :: Word
  , compressedWordValue :: Word
  } deriving (Eq, Ord, Show)

$(OverloadedRecords.overloadedRecord Default.def ''CompressedWord)

-- | Encoded as an object.
instance Aeson.ToJSON CompressedWord where
  toJSON compressedWord =
    Aeson.object
      ["Limit" .= #limit compressedWord, "Value" .= #value compressedWord]

-- | Converts a 'CompressedWord' into any integral value. This is a lossy
-- conversion because it discards the compressed word's maximum value.
fromCompressedWord
  :: (Integral a)
  => CompressedWord -> a
fromCompressedWord compressedWord = compressedWord & #value & fromIntegral
