{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Replay (Replay(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Actor as Actor
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.KeyFrame as KeyFrame
import qualified Octane.Type.Mark as Mark
import qualified Octane.Type.Message as Message
import qualified Octane.Type.Primitive.Dictionary as Dictionary
import qualified Octane.Type.Primitive.List as List
import qualified Octane.Type.Primitive.PCString as PCString
import qualified Octane.Type.Primitive.Stream as Stream
import qualified Octane.Type.Primitive.Word32LE as Word32LE
import qualified Octane.Type.Property as Property

data Replay = Replay
    { replaySize1 :: Word32LE.Word32LE
    , replayCRC1 :: Word32LE.Word32LE
    , replayVersion1 :: Word32LE.Word32LE
    , replayVersion2 :: Word32LE.Word32LE
    , replayLabel :: PCString.PCString
    , replayProperties :: Dictionary.Dictionary Property.Property
    , replaySize2 :: Word32LE.Word32LE
    , replayCRC2 :: Word32LE.Word32LE
    , replayLevels :: List.List PCString.PCString
    , replayKeyFrames :: List.List KeyFrame.KeyFrame
    , replayStream :: Stream.Stream
    , replayMessages :: List.List Message.Message
    , replayMarks :: List.List Mark.Mark
    , replayPackages :: List.List PCString.PCString
    , replayObjects :: List.List PCString.PCString
    , replayNames :: List.List PCString.PCString
    , replayActors :: List.List Actor.Actor
    , replayCacheItems :: List.List CacheItem.CacheItem
    } deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Replay where
    get =
        Replay <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get <*>
        Binary.get
    put replay = do
        replay & replaySize1 & Binary.put
        replay & replayCRC1 & Binary.put
        replay & replayVersion1 & Binary.put
        replay & replayVersion2 & Binary.put
        replay & replayLabel & Binary.put
        replay & replayProperties & Binary.put
        replay & replaySize2 & Binary.put
        replay & replayCRC2 & Binary.put
        replay & replayLevels & Binary.put
        replay & replayKeyFrames & Binary.put
        replay & replayStream & Binary.put
        replay & replayMessages & Binary.put
        replay & replayMarks & Binary.put
        replay & replayPackages & Binary.put
        replay & replayObjects & Binary.put
        replay & replayNames & Binary.put
        replay & replayActors & Binary.put
        replay & replayCacheItems & Binary.put

instance DeepSeq.NFData Replay

instance Aeson.ToJSON Replay where
    toJSON =
        Aeson.genericToJSON
            Aeson.defaultOptions
            { Aeson.fieldLabelModifier = drop 6
            }
