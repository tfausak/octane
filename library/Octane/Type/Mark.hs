{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Mark (Mark(..)) where

import Octane.Core
import Octane.Type.Primitive.PCString
import Octane.Type.Primitive.Word32LE

-- | A tick mark on the replay. The only thing that creates tick marks are
-- | goals.
data Mark = Mark
    { markLabel :: PCString
    , markFrame :: Word32LE
    } deriving (Eq, Generic, NFData, Show)

instance Binary Mark where
    get = do
        label <- get
        frame <- get
        return Mark
            { markLabel = label
            , markFrame = frame
            }

    put mark = do
        mark & markLabel & put
        mark & markFrame & put

instance ToJSON Mark where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 4 }
