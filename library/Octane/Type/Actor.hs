{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Actor (Actor(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Json as Json
import qualified Octane.Type.Primitive.PCString as PCString
import qualified Octane.Type.Primitive.Int32 as Int32

-- | A class (like @Core.Object@) and it's associated ID in the net stream
-- (like 0).
data Actor = Actor
    { actorName :: !PCString.PCString
    , actorStreamId :: !Int32.Int32
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Actor where
    get = Actor <$> Binary.get <*> Binary.get
    put actor = do
        actor & actorName & Binary.put
        actor & actorStreamId & Binary.put

instance DeepSeq.NFData Actor

instance Aeson.ToJSON Actor where
    toJSON = Aeson.genericToJSON (Json.toJsonOptions "Actor")
