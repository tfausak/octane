{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.FullReplay
    ( FullReplay
    , parseReplay
    , parseReplayFile
    , unsafeParseReplay
    , unsafeParseReplayFile
    ) where

import Data.Aeson ((.=))
import Data.Function ((&))
import Prelude ((==))

import qualified Control.DeepSeq as DeepSeq
import qualified Control.Monad as Monad
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Version as Version
import qualified GHC.Generics as Generics
import qualified Octane.Parser as Parser
import qualified Octane.Type as Type
import qualified Prelude


newtype FullReplay = FullReplay
    { unpackFullReplay :: (Type.Replay, [Parser.Frame])
    } deriving (Generics.Generic, Prelude.Show)

instance DeepSeq.NFData FullReplay

instance Aeson.ToJSON FullReplay where
    toJSON fullReplay = do
        Aeson.object
            [ "Version" .= getVersion fullReplay
            , "Metadata" .= getMetadata fullReplay
            , "Levels" .= getLevels fullReplay
            , "Messages" .= getMessages fullReplay
            , "TickMarks" .= getTickMarks fullReplay
            , "Frames" .= getFrames fullReplay
            ]


newFullReplay :: Type.Replay -> [Parser.Frame] -> FullReplay
newFullReplay replay frames = FullReplay (replay, frames)


getVersion :: FullReplay -> Prelude.String
getVersion fullReplay =
    [ fullReplay
        & unpackFullReplay
        & Prelude.fst
        & Type.replayVersion1
        & Type.unpackWord32LE
        & Prelude.fromIntegral
    , fullReplay
        & unpackFullReplay
        & Prelude.fst
        & Type.replayVersion2
        & Type.unpackWord32LE
        & Prelude.fromIntegral
    ] & Version.makeVersion & Version.showVersion


getMetadata :: FullReplay -> Map.Map Text.Text Aeson.Value
getMetadata fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayProperties
    & Type.unpackDictionary
    & Map.mapKeys Type.unpackPCString
    & Map.map Aeson.toJSON


getLevels :: FullReplay -> [Text.Text]
getLevels fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayLevels
    & Type.unpackList
    & Prelude.map Type.unpackPCString


getMessages :: FullReplay -> Map.Map Text.Text Type.PCString
getMessages fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayMessages
    & Type.unpackList
    & Prelude.map (\ message ->
        ( message
            & Type.messageFrame
            & Type.unpackWord32LE
            & Prelude.show
            & Text.pack
        , message & Type.messageContent
        ))
    & Map.fromList


getTickMarks :: FullReplay -> Map.Map Text.Text Type.PCString
getTickMarks fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayMarks
    & Type.unpackList
    & Prelude.map (\ mark ->
        ( mark
            & Type.markFrame
            & Type.unpackWord32LE
            & Prelude.show
            & Text.pack
        , mark & Type.markLabel
        ))
    & Map.fromList


getFrames :: FullReplay -> [Map.Map Text.Text Aeson.Value]
getFrames fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.snd
    & Prelude.map (\ frame -> Map.fromList
        [ ("IsKeyFrame", frame & Parser.frameIsKeyFrame & Aeson.toJSON)
        , ("Number", frame & Parser.frameNumber & Aeson.toJSON)
        , ("Time", frame & Parser.frameTime & Aeson.toJSON)
        , ("Delta", frame & Parser.frameDelta & Aeson.toJSON)
        , ("Spawned", frame
            & Parser.frameReplications
            & Prelude.filter (\ replication -> replication
                & Parser.replicationState
                & (== Parser.RSOpening))
            & Prelude.map (\ replication ->
                ( replication
                    & Parser.replicationActorId
                    & Prelude.show
                    & Text.pack
                , Aeson.object
                    [ ("Name", replication
                        & Parser.replicationObjectName
                        & Aeson.toJSON)
                    , ("Class", replication
                        & Parser.replicationClassName
                        & Aeson.toJSON)
                    , ("Position", replication
                        & Parser.replicationInitialization
                        & Prelude.fmap Parser.classInitLocation
                        & Monad.join
                        & Aeson.toJSON)
                    , ("Rotation", replication
                        & Parser.replicationInitialization
                        & Prelude.fmap Parser.classInitRotation
                        & Monad.join
                        & Aeson.toJSON)
                    ]
                ))
            & Aeson.object)
        , ("Destroyed", frame
            & Parser.frameReplications
            & Prelude.filter (\ replication -> replication
                & Parser.replicationState
                & (== Parser.RSClosing))
            & Prelude.map Parser.replicationActorId
            & Aeson.toJSON)
        ])


parseReplay :: ByteString.ByteString -> Prelude.Either Text.Text FullReplay
parseReplay bytes = do
    case Binary.decodeOrFail bytes of
        Prelude.Left (_, _, message) -> do
            Prelude.Left (Text.pack message)
        Prelude.Right (_, _, replay) -> do
            let frames = Parser.parseFrames replay
            Prelude.Right (newFullReplay replay frames)


parseReplayFile :: Prelude.FilePath -> Prelude.IO (Prelude.Either Text.Text FullReplay)
parseReplayFile file = do
    result <- Binary.decodeFileOrFail file
    case result of
        Prelude.Left (_, message) -> do
            Prelude.pure (Prelude.Left (Text.pack message))
        Prelude.Right replay -> do
            let frames = Parser.parseFrames replay
            Prelude.pure (Prelude.Right (newFullReplay replay frames))


unsafeParseReplay :: ByteString.ByteString -> FullReplay
unsafeParseReplay bytes = do
    let replay = Binary.decode bytes
    let frames = Parser.parseFrames replay
    newFullReplay replay frames


unsafeParseReplayFile :: Prelude.FilePath -> Prelude.IO FullReplay
unsafeParseReplayFile file = do
    replay <- Binary.decodeFile file
    let frames = Parser.parseFrames replay
    Prelude.pure (newFullReplay replay frames)
