module Octane.Data.GameModes where

import Data.Function ((&))

import qualified Data.Bimap as Bimap
import qualified Data.Text as StrictText


gameModes :: Bimap.Bimap Int StrictText.Text
gameModes =
    [ ("Soccar", 0)
    , ("Hockey", 1)
    , ("Hoops", 2)
    ] & map (\ (v, k) -> (k, StrictText.pack v)) & Bimap.fromList
