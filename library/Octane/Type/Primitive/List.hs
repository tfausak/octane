{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.List (List(..)) where

import qualified Control.Monad as Monad
import Octane.Core
import Octane.Type.Primitive.Word32LE

-- | A length-prefixed list.
newtype List a = List [a]
    deriving (Eq, Generic, NFData, Show)

instance (Binary a) => Binary (List a) where
    get = do
        (Word32LE size) <- get
        elements <- Monad.replicateM (fromIntegral size) get
        elements & pack & return

    put list = do
        list & unpack & length & fromIntegral & Word32LE & put
        list & unpack & mapM_ put

instance Newtype (List a)

instance (ToJSON a) => ToJSON (List a)
