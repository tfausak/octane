{-# LANGUAGE OverloadedStrings #-}

module Octane.Parser.Types.Mark where

import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Binary as Binary
import Data.Function ((&))
import Octane.Parser.Types.Int32LE
import Octane.Parser.Types.PCString

data Mark = NewMark {
    markLabel :: PCString,
    markFrame :: Int32LE
} deriving (Show)

instance Aeson.ToJSON Mark where
    toJSON mark = Aeson.object [
        "id" .= markLabel mark,
        "frame" .= markFrame mark
        ]

instance Binary.Binary Mark where
    get = do
        label <- Binary.get
        frame <- Binary.get
        return NewMark {
            markLabel = label,
            markFrame = frame
        }

    put mark = do
        mark & markLabel & Binary.put
        mark & markFrame & Binary.put
