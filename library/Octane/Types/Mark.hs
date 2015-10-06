module Octane.Types.Mark where

import Octane.Types.Int32LE (Int32LE)
import Octane.Types.PCString (PCString)

import qualified Data.Binary as B

data Mark = NewMark
    { markLabel :: PCString
    , markFrame :: Int32LE
    } deriving (Show)

instance B.Binary Mark where
    get = NewMark
        <$> B.get
        <*> B.get

    put mark = do
        B.put (markLabel mark)
        B.put (markFrame mark)
