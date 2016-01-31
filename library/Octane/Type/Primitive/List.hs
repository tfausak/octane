module Octane.Type.Primitive.List (List(..)) where

import Octane.Core
import Octane.Type.Primitive.Int32LE

newtype List a = NewList
    { getList :: [a]
    } deriving (Show)

instance (Binary a) => Binary (List a) where
    get = do
        (NewInt32LE size) <- get
        elements <- replicateM (fromIntegral size) get
        elements & NewList & return

    put (NewList list) = do
        list & length & fromIntegral & NewInt32LE & put
        list & mapM_ put
