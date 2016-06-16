{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Primitive.List (List(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Int32 as Int32

-- | A length-prefixed list.
newtype List a = List
    { unpackList :: [a]
    } deriving (Eq,Generics.Generic,Show)

instance (Binary.Binary a) => Binary.Binary (List a) where
    get = do
        (Int32.Int32 size) <- Binary.get
        elements <- Monad.replicateM (fromIntegral size) Binary.get
        elements & List & return
    put list = do
        list & unpackList & length & fromIntegral & Int32.Int32 &
            Binary.put
        list & unpackList & mapM_ Binary.put

instance (DeepSeq.NFData a) => DeepSeq.NFData (List a)

instance (Aeson.ToJSON a) => Aeson.ToJSON (List a) where
    toJSON list = list & unpackList & Aeson.toJSON
