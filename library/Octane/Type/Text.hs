{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Text
  ( Text(..)
  , encodeLatin1
  ) where

import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as StrictBytes
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.String as String
import qualified Data.Text as StrictText

-- | A thin wrapper around 'StrictText.Text'.
newtype Text = Text
  { textUnpack :: StrictText.Text
  } deriving (Eq, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Text)

-- | Allows you to write 'Text' as string literals with @OverloadedStrings@.
-- Also allows using the 'String.fromString' helper function.
instance String.IsString Text where
  fromString string = Text (StrictText.pack string)

-- | Shown as a string literal, like @"this"@.
instance Show Text where
  show text = show (#unpack text)

-- | Encoded directly as a JSON string.
instance Aeson.ToJSON Text where
  toJSON text = text & #unpack & Aeson.toJSON

-- | Encodes text as Latin-1. Note that this isn't really safe if the text has
-- characters that can't be encoded in Latin-1.
encodeLatin1 :: StrictText.Text -> StrictBytes.ByteString
encodeLatin1 text = text & StrictText.unpack & StrictBytes.pack
