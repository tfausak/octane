{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Utility.Generator
  ( generateStream
  ) where

import Data.Function ((&))

import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Put as Binary
import qualified Data.Default.Class as Default
import qualified Data.Map.Strict as Map
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Data.Tuple as Tuple
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CacheItem as CacheItem
import qualified Octane.Type.ClassItem as ClassItem
import qualified Octane.Type.Frame as Frame
import qualified Octane.Type.Initialization as Initialization
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.List as List
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State
import qualified Octane.Type.Stream as Stream
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Value as Value

data Context = Context
  { contextObjectMap :: Map.Map StrictText.Text Int32.Int32
  }

$(OverloadedRecords.overloadedRecord Default.def ''Context)

-- | Generates a network stream.
generateStream
  :: [Frame.Frame]
  -> List.List Text.Text
  -> List.List Text.Text
  -> List.List ClassItem.ClassItem
  -> List.List CacheItem.CacheItem
  -> Stream.Stream
generateStream frames objects _names _classes _cache = do
  let objectMap =
        objects & #unpack & map #unpack & zip [0 ..] & map Tuple.swap &
        Map.fromList
  let context = Context objectMap
  let bitPut = putFrames context frames
  let bytePut = BinaryBit.runBitPut bitPut
  let bytes = Binary.runPut bytePut
  Stream.Stream bytes

putFrames :: Context -> [Frame.Frame] -> BinaryBit.BitPut ()
putFrames context frames = do
  case frames of
    [] -> pure ()
    frame:rest -> do
      putFrame context frame
      putFrames context rest

putFrame :: Context -> Frame.Frame -> BinaryBit.BitPut ()
putFrame context frame = do
  frame & #time & BinaryBit.putBits 32
  frame & #delta & BinaryBit.putBits 32
  frame & #replications & putReplications context

putReplications :: Context -> [Replication.Replication] -> BinaryBit.BitPut ()
putReplications context replications = do
  case replications of
    [] -> do
      False & Boolean.Boolean & BinaryBit.putBits 1
    replication:rest -> do
      True & Boolean.Boolean & BinaryBit.putBits 1
      putReplication context replication
      putReplications context rest

putReplication :: Context -> Replication.Replication -> BinaryBit.BitPut ()
putReplication context replication = do
  replication & #actorId & BinaryBit.putBits 0
  case #state replication of
    State.Opening -> putNewReplication context replication
    State.Existing -> putExistingReplication context replication
    State.Closing -> putClosedReplication

putNewReplication :: Context -> Replication.Replication -> BinaryBit.BitPut ()
putNewReplication context replication = do
  True & Boolean.Boolean & BinaryBit.putBits 1 -- open
  True & Boolean.Boolean & BinaryBit.putBits 1 -- new
  False & Boolean.Boolean & BinaryBit.putBits 1 -- unknown
  let objectName = #objectName replication
  case Map.lookup objectName (#objectMap context) of
    Nothing -> fail ("could not find object id for name " ++ show objectName)
    Just objectId -> BinaryBit.putBits 0 objectId
  case #initialization replication of
    Nothing -> pure ()
    Just x -> Initialization.putInitialization x

putExistingReplication :: Context
                       -> Replication.Replication
                       -> BinaryBit.BitPut ()
putExistingReplication context replication = do
  True & Boolean.Boolean & BinaryBit.putBits 1 -- open
  False & Boolean.Boolean & BinaryBit.putBits 1 -- existing
  replication & #properties & Map.toAscList & mapM_ (putProperty context)

putClosedReplication :: BinaryBit.BitPut ()
putClosedReplication = do
  False & Boolean.Boolean & BinaryBit.putBits 1 -- closed

putProperty :: Context -> (StrictText.Text, Value.Value) -> BinaryBit.BitPut ()
putProperty _context (_name, _value) = do
  True & Boolean.Boolean & BinaryBit.putBits 1 -- has property
  pure () -- TODO: get property id for name and put it
  pure () -- TODO: put property value
