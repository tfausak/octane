module Octane.Types.List where

import Octane.Types.Int32LE (Int32LE (NewInt32LE))

import Control.Monad (replicateM)

import qualified Data.Binary as B

newtype List a = NewList
    { getList :: [a]
    } deriving (Show)

instance (B.Binary a) => B.Binary (List a) where
    get = do
        (NewInt32LE size) <- B.get
        elements <- replicateM size B.get
        return (NewList elements)

    put (NewList list) = do
        B.put (NewInt32LE (length list))
        mapM_ B.put list
