module Octane.Data.GameModes (gameModes) where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


-- | A one-to-one mapping between game mode IDs and their names.
gameModes :: Bimap.Bimap Int StrictText.Text
gameModes =
    [ ("Soccar", 0)
    , ("Hockey", 1)
    , ("Hoops", 2)
    ] & map (\ (v, k) -> (k, StrictText.pack v)) & Bimap.fromList
