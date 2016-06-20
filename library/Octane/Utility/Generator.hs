module Octane.Utility.Generator (generateStream) where

import Data.Function ((&))

import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.ClassItem as ClassItem
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.List as List
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.Stream as Stream
import qualified Octane.Type.Text as Text


-- | Generates a network stream.
generateStream
    :: [Frame.Frame]
    -> List.List Text.Text
    -> List.List Text.Text
    -> List.List ClassItem.ClassItem
    -> List.List CacheItem.CacheItem
    -> Stream.Stream
generateStream frames _objects _names _classes _cache = do
    let bitPut = putFrames frames
    let bytePut = BinaryBit.runBitPut bitPut
    let bytes = Binary.runPut bytePut
    Stream.Stream bytes


putFrames :: [Frame.Frame] -> BinaryBit.BitPut ()
putFrames frames = do
    case frames of
        [] -> pure ()
        frame : rest -> do
            putFrame frame
            putFrames rest


putFrame :: Frame.Frame -> BinaryBit.BitPut ()
putFrame frame = do
    frame & Frame.time & BinaryBit.putBits 32
    frame & Frame.delta & BinaryBit.putBits 32
    frame & Frame.replications & putReplications


putReplications :: [Replication.Replication] -> BinaryBit.BitPut ()
putReplications replications = do
    case replications of
        [] -> do
            False & Boolean.Boolean & BinaryBit.putBits 1
        replication : rest -> do
            True & Boolean.Boolean & BinaryBit.putBits 1
            putReplication replication
            putReplications rest


putReplication :: Replication.Replication -> BinaryBit.BitPut ()
putReplication replication = do
    replication & Replication.actorId & putActorId
    case Replication.state replication of
        _ -> pure ()


--


putActorId :: Word -> BinaryBit.BitPut ()
putActorId actorId = putCompressedWord maxActorId actorId


maxActorId :: Word
maxActorId = 1024


putCompressedWord :: Word -> Word -> BinaryBit.BitPut ()
putCompressedWord _maxValue _value = do
    pure () -- TODO
