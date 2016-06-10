{-# LANGUAGE DeriveGeneric #-}

module Octane.FullReplay
    ( FullReplay
    , parseReplay
    , parseReplayFile
    , unsafeParseReplay
    , unsafeParseReplayFile
    ) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Octane.Parser as Parser
import qualified Octane.Type as Type
import qualified Prelude


newtype FullReplay = FullReplay (Type.Replay, [Parser.Frame])
    deriving (Generics.Generic, Prelude.Show)

instance DeepSeq.NFData FullReplay

instance Aeson.ToJSON FullReplay where
    toJSON _ = Aeson.object
        [
        ]


fullReplay :: Type.Replay -> [Parser.Frame] -> FullReplay
fullReplay replay frames = FullReplay (replay, frames)


parseReplay :: ByteString.ByteString -> Prelude.Either Text.Text FullReplay
parseReplay bytes = do
    case Binary.decodeOrFail bytes of
        Prelude.Left (_, _, message) -> do
            Prelude.Left (Text.pack message)
        Prelude.Right (_, _, replay) -> do
            let frames = Parser.parseFrames replay
            Prelude.Right (fullReplay replay frames)


parseReplayFile :: Prelude.FilePath -> Prelude.IO (Prelude.Either Text.Text FullReplay)
parseReplayFile file = do
    result <- Binary.decodeFileOrFail file
    case result of
        Prelude.Left (_, message) -> do
            Prelude.pure (Prelude.Left (Text.pack message))
        Prelude.Right replay -> do
            let frames = Parser.parseFrames replay
            Prelude.pure (Prelude.Right (fullReplay replay frames))


unsafeParseReplay :: ByteString.ByteString -> FullReplay
unsafeParseReplay bytes = do
    let replay = Binary.decode bytes
    let frames = Parser.parseFrames replay
    fullReplay replay frames


unsafeParseReplayFile :: Prelude.FilePath -> Prelude.IO FullReplay
unsafeParseReplayFile file = do
    replay <- Binary.decodeFile file
    let frames = Parser.parseFrames replay
    Prelude.pure (fullReplay replay frames)
