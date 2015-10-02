module Octane.Types.ObjectMap where

import Octane.Types.List (List (NewList))
import Octane.Types.PCString (PCString)

import qualified Data.Binary as B
import qualified Data.IntMap as M

newtype ObjectMap = NewObjectMap
    { unObjectMap :: M.IntMap PCString
    } deriving (Show)

instance B.Binary ObjectMap where
    get = do
        NewList list <- B.get
        return (NewObjectMap (M.fromDistinctAscList (zip [0 ..] list)))

    put (NewObjectMap objectMap) = do
        B.put (NewList (M.elems objectMap))
