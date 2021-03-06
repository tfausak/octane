{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Frame
  ( Frame(..)
  ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Data.Aeson as Aeson
import qualified Data.Default.Class as Default
import qualified Data.Map.Strict as Map
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Text as StrictText
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Replication as Replication
import qualified Octane.Type.State as State

-- | A frame in the network stream. This holds all the interesting game data.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it
-- requires out-of-band information (the class property map) to decode.
data Frame = Frame
  { frameNumber :: Word
    -- ^ This frame's number in the network stream. Starts at 0.
  , frameIsKeyFrame :: Bool
    -- ^ Is this frame a key frame?
  , frameTime :: Float32.Float32
    -- ^ The since the start of the match that this frame occurred.
  , frameDelta :: Float32.Float32
    -- ^ The time between the last frame and this one.
  , frameReplications :: [Replication.Replication]
    -- ^ A list of all the replications in this frame.
  } deriving (Eq, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Frame)

instance Aeson.ToJSON Frame where
  toJSON frame =
    Aeson.object
      [ "Number" .= #number frame
      , "IsKeyFrame" .= #isKeyFrame frame
      , "Time" .= #time frame
      , "Delta" .= #delta frame
      , "Spawned" .= (frame & #replications & getSpawned)
      , "Updated" .= (frame & #replications & getUpdated)
      , "Destroyed" .= (frame & #replications & getDestroyed)
      ]

newtype Spawned =
  Spawned [Replication.Replication]

instance Aeson.ToJSON Spawned where
  toJSON (Spawned xs) =
    xs &
    map
      (\x -> do
         let k = x & #actorId & #value & show & StrictText.pack
         let v =
               Aeson.object
                 [ "Name" .= #objectName x
                 , "Class" .= #className x
                 , "Position" .= (x & #initialization & fmap #location)
                 , "Rotation" .= (x & #initialization & fmap #rotation)
                 ]
         (k, v)) &
    Map.fromList &
    Aeson.toJSON

getSpawned :: [Replication.Replication] -> Spawned
getSpawned xs = xs & filter (\x -> x & #state & State.isOpening) & Spawned

newtype Updated =
  Updated [Replication.Replication]

instance Aeson.ToJSON Updated where
  toJSON (Updated xs) =
    xs &
    map
      (\x -> do
         let k = x & #actorId & #value & show & StrictText.pack
         let v = x & #properties
         (k, v)) &
    Map.fromList &
    Aeson.toJSON

getUpdated :: [Replication.Replication] -> Updated
getUpdated xs =
  xs & filter (\x -> x & #state & State.isExisting) &
  filter (\x -> x & #properties & null & not) &
  Updated

newtype Destroyed =
  Destroyed [Replication.Replication]

instance Aeson.ToJSON Destroyed where
  toJSON (Destroyed xs) = xs & map #actorId & map #value & Aeson.toJSON

getDestroyed :: [Replication.Replication] -> Destroyed
getDestroyed xs = xs & filter (\x -> x & #state & State.isClosing) & Destroyed
