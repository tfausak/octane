{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Octane.Type.List (List(..)) where

import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import qualified Data.Vector as Vector
import qualified GHC.Generics as Generics
import qualified Octane.Type.Word32 as Word32


-- | A list of valeus.
newtype List a = List
    { unpack :: [a]
    } deriving (Eq, Generics.Generic, Ord, Show)

-- | Bytewise lists are length-prefixed.
instance (Binary.Binary a) => Binary.Binary (List a) where
    get = do
        size <- Binary.get
        elements <- Monad.replicateM (Word32.fromWord32 size) Binary.get
        elements & List & return

    put list = do
        list & unpack & length & fromIntegral & Word32.Word32 & Binary.put
        list & unpack & mapM_ Binary.put

instance (Aeson.FromJSON a) => Aeson.FromJSON (List a) where
    parseJSON json = case json of
        Aeson.Array array -> array & Vector.toList & mapM Aeson.parseJSON & fmap List
        _ -> Aeson.typeMismatch "List" json

instance (DeepSeq.NFData a) => DeepSeq.NFData (List a) where

instance (Aeson.ToJSON a) => Aeson.ToJSON (List a) where
    toJSON list = list & unpack & Aeson.toJSON
