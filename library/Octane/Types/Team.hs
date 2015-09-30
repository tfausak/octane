{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Team where

import Octane.Types.PCString (PCString)

import qualified Data.Binary as B

data Team
    = Blue
    | Orange
    deriving (Show)

instance B.Binary Team where
    get = do
        kind <- B.get
        case kind :: PCString of
            "Team0Goal" -> return Blue
            "Team1Goal" -> return Orange
            _ -> fail ("unknown goal type " ++ show kind)

    put team = do
        let kind = case team of
                Blue -> "Team0Goal"
                Orange -> "Team1Goal"
        B.put (kind :: PCString)
