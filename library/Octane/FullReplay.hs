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

import qualified Control.DeepSeq as DeepSeq
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
            [ "Version" .= version fullReplay
            , "Metadata" .= metadata fullReplay
            , "Levels" .= levels fullReplay
            ]


newFullReplay :: Type.Replay -> [Parser.Frame] -> FullReplay
newFullReplay replay frames = FullReplay (replay, frames)


version :: FullReplay -> Prelude.String
version fullReplay =
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


metadata :: FullReplay -> Map.Map Text.Text Aeson.Value
metadata fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayProperties
    & Type.unpackDictionary
    & Map.mapKeys Type.unpackPCString
    & Map.map Aeson.toJSON


levels :: FullReplay -> [Text.Text]
levels fullReplay = fullReplay
    & unpackFullReplay
    & Prelude.fst
    & Type.replayLevels
    & Type.unpackList
    & Prelude.map Type.unpackPCString


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
