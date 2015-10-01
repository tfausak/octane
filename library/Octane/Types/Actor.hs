module Octane.Types.Actor where

import Octane.Types.Int32LE (Int32LE)
import Octane.Types.PCString (PCString)

import qualified Data.Binary as B

data Actor = NewActor
    { actorName :: PCString
    , actorTag :: Int32LE
    } deriving (Show)

instance B.Binary Actor where
    get = NewActor
        <$> B.get
        <*> B.get

    put actor = do
        B.put (actorName actor)
        B.put (actorTag actor)
