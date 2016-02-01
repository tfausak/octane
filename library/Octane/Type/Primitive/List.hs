{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Octane.Type.Primitive.List (List(..)) where

import Octane.Core
import Octane.Type.Primitive.Int32LE

-- | A length-prefixed list.
newtype List a = List
    { getList :: [a]
    } deriving (Eq, Generic, NFData, Show)

instance (Binary a) => Binary (List a) where
    get = do
        (Int32LE size) <- get
        elements <- replicateM (fromIntegral size) get
        elements & List & return

    put (List list) = do
        list & length & fromIntegral & Int32LE & put
        list & mapM_ put
