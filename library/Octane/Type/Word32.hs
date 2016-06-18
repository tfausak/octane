{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Word32 (Word32(..), fromWord32, toWord32) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Word as Word
import qualified GHC.Generics as Generics
import qualified Text.Printf as Printf


-- | A 32-bit unsigned integer.
newtype Word32 = Word32
    { unpack :: Word.Word32
    } deriving (Eq, Generics.Generic, Num, Ord)

-- | Stored in little-endian byte order.
instance Binary.Binary Word32 where
    get = do
        value <- Binary.getWord32le
        pure (Word32 value)

    put word32 = do
        let value = unpack word32
        Binary.putWord32le value

instance DeepSeq.NFData Word32 where

-- | Shown as @0x01020304@.
instance Show Word32 where
    show word32 = Printf.printf "0x%08x" (unpack word32)

instance Aeson.ToJSON Word32 where
    toJSON word32 = word32
        & unpack
        & Aeson.toJSON


-- | Converts a 'Word32' into any 'Integral' value.
--
-- >>> fromWord32 0x00000001 :: Word.Word32
-- 1
--
-- >>> fromWord32 0xffffffff :: Data.Int.Int32
-- -1
fromWord32 :: (Integral a) => Word32 -> a
fromWord32 word32 = fromIntegral (unpack word32)


-- | Converts any 'Integral' value into a 'Word32'.
--
-- >>> toWord32 (1 :: Word.Word32)
-- 0x00000001
--
-- >>> toWord32 (-1 :: Data.Int.Int32)
-- 0xffffffff
toWord32 :: (Integral a) => a -> Word32
toWord32 value = Word32 (fromIntegral value)
