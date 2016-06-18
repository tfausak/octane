{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.List (List(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Boolean as Boolean
import qualified Octane.Type.Primitive.Word32 as Word32


-- | A list of valeus.
newtype List a = List
    { unpackList :: [a]
    } deriving (Eq, Generics.Generic, Show)

-- | Bytewise lists are length-prefixed.
instance (Binary.Binary a) => Binary.Binary (List a) where
    get = do
        size <- Binary.get
        elements <- Monad.replicateM (Word32.fromWord32 size) Binary.get
        elements & List & return

    put list = do
        list & unpackList & length & fromIntegral & Word32.Word32 & Binary.put
        list & unpackList & mapM_ Binary.put

-- | Bitwise lists use a bit to signify if there is another element.
instance (BinaryBit.BinaryBit a) => BinaryBit.BinaryBit (List a) where
    getBits _ = do
        (Boolean.Boolean hasMore) <- BinaryBit.getBits 0
        if hasMore
            then do
                x <- BinaryBit.getBits 0
                (List xs) <- BinaryBit.getBits 0
                pure (List (x : xs))
            else pure (List [])

    putBits _ list = case unpackList list of
        [] -> BinaryBit.putBits 0 (Boolean.Boolean False)
        x : xs -> do
            BinaryBit.putBits 0 (Boolean.Boolean True)
            BinaryBit.putBits 0 x
            BinaryBit.putBits 0 (List xs)

instance (Aeson.FromJSON a) => Aeson.FromJSON (List a) where
    parseJSON json = case json of
        Aeson.Array array -> do
            values <- Vector.mapM Aeson.parseJSON array
            values & Vector.toList & List & pure
        _ -> Aeson.typeMismatch "List" json

instance (DeepSeq.NFData a) => DeepSeq.NFData (List a)

instance (Aeson.ToJSON a) => Aeson.ToJSON (List a) where
    toJSON list = list & unpackList & Aeson.toJSON
