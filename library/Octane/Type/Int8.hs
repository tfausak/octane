{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Int8 (Int8(..), fromInt8, toInt8) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Int as Int
import qualified GHC.Generics as Generics


-- | A 8-bit signed integer.
newtype Int8 = Int8
    { unpack :: Int.Int8
    } deriving (Eq, Generics.Generic, Num, Ord)

instance Binary.Binary Int8 where
    get = do
        value <- Binary.getInt8
        pure (Int8 value)

    put int8 = do
        let value = unpack int8
        Binary.putInt8 value

instance Aeson.FromJSON Int8 where
    parseJSON json = do
        value <- Aeson.parseJSON json
        pure (Int8 value)

instance DeepSeq.NFData Int8 where

-- | Shown as @1234@.
instance Show Int8 where
    show int8 = show (unpack int8)

instance Aeson.ToJSON Int8 where
    toJSON int8 = int8
        & unpack
        & Aeson.toJSON


-- | Converts a 'Int8' into any 'Integral' value.
--
-- >>> fromInt8 1 :: Int.Int8
-- 1
fromInt8 :: (Integral a) => Int8 -> a
fromInt8 int8 = fromIntegral (unpack int8)


-- | Converts any 'Integral' value into a 'Int8'.
--
-- >>> toInt8 (1 :: Int.Int8)
-- 1
toInt8 :: (Integral a) => a -> Int8
toInt8 value = Int8 (fromIntegral value)
