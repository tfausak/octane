module Octane.Type.Mark (Mark(..)) where

import Octane.Core
import Octane.Type.Int32LE
import Octane.Type.PCString

data Mark = NewMark
    { markLabel :: PCString
    , markFrame :: Int32LE
    } deriving (Show)

instance Binary Mark where
    get = do
        label <- get
        frame <- get
        return NewMark
            { markLabel = label
            , markFrame = frame
            }

    put mark = do
        mark & markLabel & put
        mark & markFrame & put
