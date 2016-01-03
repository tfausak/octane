module Octane.Types.ObjectMap where

import qualified Data.IntMap as IntMap
import Octane.Core
import Octane.Types.List
import Octane.Types.PCString

newtype ObjectMap = NewObjectMap
    { getObjectMap :: IntMap PCString
    } deriving (Show)

instance Binary ObjectMap where
    get = do
        NewList objects <- get
        objects & zip [0 ..] & IntMap.fromList & NewObjectMap & return

    put (NewObjectMap objects) = do
        objects & IntMap.elems & NewList & put
