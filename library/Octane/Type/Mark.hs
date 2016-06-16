{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Mark (Mark(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Json as Json
import qualified Octane.Type.Primitive.Text as Text
import qualified Octane.Type.Primitive.Int32 as Int32

-- | A tick mark on the replay. Both goals and saves make tick marks.
data Mark = Mark
    { markLabel :: !Text.Text
    , markFrame :: !Int32.Int32
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Mark where
    get = Mark <$> Binary.get <*> Binary.get
    put mark = do
        mark & markLabel & Binary.put
        mark & markFrame & Binary.put

instance DeepSeq.NFData Mark

instance Aeson.ToJSON Mark where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "Mark")
