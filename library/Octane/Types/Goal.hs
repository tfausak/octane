module Octane.Types.Goal where

import Octane.Types.Int32LE (Int32LE)
import Octane.Types.Team (Team)

import qualified Data.Binary as B

data Goal = NewGoal
    { goalTeam :: Team
    , goalFrame :: Int32LE
    } deriving (Show)

instance B.Binary Goal where
    get = do
        team <- B.get
        frame <- B.get
        return NewGoal
            { goalTeam = team
            , goalFrame = frame
            }

    put goal = do
        B.put (goalTeam goal)
        B.put (goalFrame goal)
