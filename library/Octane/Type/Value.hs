{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Value (Value(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified GHC.Generics as Generics
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.CompressedWord as CompressedWord
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.RemoteId as RemoteId
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Vector as Vector
import qualified Octane.Type.Word16 as Word16
import qualified Octane.Type.Word32 as Word32
import qualified Octane.Type.Word64 as Word64
import qualified Octane.Type.Word8 as Word8


-- | A replicated property's value.
data Value
    = VBoolean
        Boolean.Boolean
    | VByte
        Word8.Word8
    | VCamSettings
        Float32.Float32
        Float32.Float32
        Float32.Float32
        Float32.Float32
        Float32.Float32
        Float32.Float32
    | VDemolish
        Boolean.Boolean
        Word32.Word32
        Boolean.Boolean
        Word32.Word32
        (Vector.Vector Int)
        (Vector.Vector Int)
    | VEnum
        Word16.Word16
        Boolean.Boolean
    | VExplosion
        Boolean.Boolean
        (Maybe Int32.Int32)
        (Vector.Vector Int)
    | VFlaggedInt
        Boolean.Boolean
        Int32.Int32
    | VFloat
        Float32.Float32
    | VGameMode
        Word8.Word8
    | VInt
        Int32.Int32
    | VLoadout
        Word8.Word8
        Word32.Word32
        Word32.Word32
        Word32.Word32
        Word32.Word32
        Word32.Word32
        Word32.Word32
        Word32.Word32
        (Maybe Word32.Word32)
    | VLoadoutOnline
        [[(Word32.Word32, CompressedWord.CompressedWord)]]
    | VLocation
        (Vector.Vector Int)
    | VMusicStinger
        Boolean.Boolean
        Word32.Word32
        Word8.Word8
    | VPickup
        Boolean.Boolean
        (Maybe Word32.Word32)
        Boolean.Boolean
    | VPrivateMatchSettings
        Text.Text
        Word32.Word32
        Word32.Word32
        Text.Text
        Text.Text
        Boolean.Boolean
    | VQWord
        Word64.Word64
    | VRelativeRotation
        (Vector.Vector Float)
    | VReservation
        CompressedWord.CompressedWord
        Word8.Word8
        RemoteId.RemoteId
        (Maybe Word8.Word8)
        (Maybe Text.Text)
        Boolean.Boolean
        Boolean.Boolean
    | VRigidBodyState
        Boolean.Boolean
        (Vector.Vector Int)
        (Vector.Vector Float)
        (Maybe (Vector.Vector Int))
        (Maybe (Vector.Vector Int))
    | VString
        Text.Text
    | VTeamPaint
        Word8.Word8
        Word8.Word8
        Word8.Word8
        Word32.Word32
        Word32.Word32
    | VUniqueId
        Word8.Word8
        RemoteId.RemoteId
        (Maybe Word8.Word8)
    deriving (Eq, Generics.Generic, Show)

instance DeepSeq.NFData Value where
