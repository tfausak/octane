module Octane.Type.Word16 (Word16(..), fromWord16, toWord16) where

import Basics

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Word as Word
import qualified Text.Printf as Printf


-- | A 16-bit unsigned integer.
newtype Word16 = Word16
    { word16Unpack :: Word.Word16
    } deriving (Eq, Generic, Num, Ord)

$(OverloadedRecords.overloadedRecord Default.def ''Word16)

-- | Little-endian.
instance Binary.Binary Word16 where
    get = do
        value <- Binary.getWord16le
        pure (Word16 value)

    put word16 = do
        let value = #unpack word16
        Binary.putWord16le value

instance NFData Word16 where

-- | Shown as @0x0102@.
instance Show Word16 where
    show word16 = Printf.printf "0x%04x" (#unpack word16)

-- | Encoded as a JSON number.
instance Aeson.ToJSON Word16 where
    toJSON word16 = word16
        & #unpack
        & Aeson.toJSON


-- | Converts a 'Word16' into any 'Integral' value.
fromWord16 :: (Integral a) => Word16 -> a
fromWord16 word16 = fromIntegral (#unpack word16)


-- | Converts any 'Integral' value into a 'Word16'.
toWord16 :: (Integral a) => a -> Word16
toWord16 value = Word16 (fromIntegral value)
