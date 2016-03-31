{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Octane.Type.Replay (Replay(..)) where

import Octane.Internal.Core
import Octane.Type.Actor
import Octane.Type.CacheItem
import Octane.Type.KeyFrame
import Octane.Type.Mark
import Octane.Type.Message
import Octane.Type.Primitive.Dictionary
import Octane.Type.Primitive.List
import Octane.Type.Primitive.PCString
import Octane.Type.Primitive.Stream
import Octane.Type.Primitive.Word32LE
import Octane.Type.Property

data Replay = Replay
    { replaySize1 :: Word32LE
    , replayCRC1 :: Word32LE
    , replayVersion1 :: Word32LE
    , replayVersion2 :: Word32LE
    , replayLabel :: PCString
    , replayProperties :: Dictionary Property
    , replaySize2 :: Word32LE
    , replayCRC2 :: Word32LE
    , replayLevels :: List PCString
    , replayKeyFrames :: List KeyFrame
    , replayStream :: Stream
    , replayMessages :: List Message
    , replayMarks :: List Mark
    , replayPackages :: List PCString
    , replayObjects :: List PCString
    , replayNames :: List PCString
    , replayActors :: List Actor
    , replayCacheItems :: List CacheItem
    } deriving (Eq, Generic, NFData, Show)

instance Binary Replay where
    get = Replay
        <$> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get
        <*> get

    put replay = do
        replay & replaySize1 & put
        replay & replayCRC1 & put
        replay & replayVersion1 & put
        replay & replayVersion2 & put
        replay & replayLabel & put
        replay & replayProperties & put
        replay & replaySize2 & put
        replay & replayCRC2 & put
        replay & replayLevels & put
        replay & replayKeyFrames & put
        replay & replayStream & put
        replay & replayMessages & put
        replay & replayMarks & put
        replay & replayPackages & put
        replay & replayObjects & put
        replay & replayNames & put
        replay & replayActors & put
        replay & replayCacheItems & put

instance ToJSON Replay where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 6 }
