{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Mark (Mark(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.PCString as PCString
import qualified Octane.Type.Primitive.Word32LE as Word32LE

-- | A tick mark on the replay. Both goals and saves make tick marks.
data Mark = Mark
    { markLabel :: PCString.PCString
    , markFrame :: Word32LE.Word32LE
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Mark where
    get = Mark <$> Binary.get <*> Binary.get
    put mark = do
        mark & markLabel & Binary.put
        mark & markFrame & Binary.put

instance DeepSeq.NFData Mark

instance Aeson.ToJSON Mark where
    toJSON =
        Aeson.genericToJSON
            Aeson.defaultOptions
            { Aeson.fieldLabelModifier = drop 4
            }
