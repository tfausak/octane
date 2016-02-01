module Octane.Type.Mark (Mark(..)) where

import Octane.Core
import Octane.Type.Primitive.PCString
import Octane.Type.Primitive.Int32LE

data Mark = NewMark
    { markLabel :: PCString
    , markFrame :: Int32LE
    } deriving (Eq, Show)

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
