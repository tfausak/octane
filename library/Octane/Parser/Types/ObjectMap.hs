module Octane.Parser.Types.ObjectMap where

import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified Data.IntMap as IntMap
import Octane.Parser.Types.List
import Octane.Parser.Types.PCString

newtype ObjectMap = NewObjectMap {
    getObjectMap :: IntMap.IntMap PCString
} deriving (Show)

instance Aeson.ToJSON ObjectMap where
    toJSON (NewObjectMap objectMap) = Aeson.toJSON objectMap

instance Binary.Binary ObjectMap where
    get = do
        NewList objects <- Binary.get
        objects & zip [0 ..] & IntMap.fromList & NewObjectMap & return

    put (NewObjectMap objects) = do
        objects & IntMap.elems & NewList & Binary.put
