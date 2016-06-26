{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Word16 (Word16(..), fromWord16, toWord16) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Text.Printf as Printf


-- | A 16-bit unsigned integer.
newtype Word16 = Word16
    { unpack :: Word.Word16
    } deriving (Eq, Generics.Generic, Num, Ord)

-- | >>> Binary.decode "\x01\x00" :: Word16
-- 0x0001
--
-- >>> Binary.encode (1 :: Word16)
-- "\SOH\NUL"
instance Binary.Binary Word16 where
    get = do
        value <- Binary.getWord16le
        pure (Word16 value)

    put word16 = do
        let value = unpack word16
        Binary.putWord16le value

instance DeepSeq.NFData Word16 where

-- | Shown as @0x0102@.
--
-- >>> show (1 :: Word16)
-- "0x0001"
instance Show Word16 where
    show word16 = Printf.printf "0x%04x" (unpack word16)

-- | Encoded as a JSON number.
--
-- >>> Aeson.encode (1 :: Word16)
-- "1"
instance Aeson.ToJSON Word16 where
    toJSON word16 = word16
        & unpack
        & Aeson.toJSON


-- | Converts a 'Word16' into any 'Integral' value.
--
-- >>> fromWord16 0x0001 :: Word.Word16
-- 1
--
-- >>> fromWord16 0xffff :: Data.Int.Int16
-- -1
fromWord16 :: (Integral a) => Word16 -> a
fromWord16 word16 = fromIntegral (unpack word16)


-- | Converts any 'Integral' value into a 'Word16'.
--
-- >>> toWord16 (1 :: Word.Word16)
-- 0x0001
--
-- >>> toWord16 (-1 :: Data.Int.Int16)
-- 0xffff
toWord16 :: (Integral a) => a -> Word16
toWord16 value = Word16 (fromIntegral value)
