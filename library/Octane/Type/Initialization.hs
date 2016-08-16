{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StrictData #-}

module Octane.Type.Initialization
    ( Initialization(..)
    , getInitialization
    , putInitialization
    ) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified GHC.Generics as Generics
import qualified Octane.Data as Data
import qualified Octane.Type.Int8 as Int8
import qualified Octane.Type.Vector as Vector


-- | Information about a new instance of a class.
--
-- This cannot be an instance of 'Data.Binary.Bits.BinaryBit' because it
-- requires out-of-band information (the class name) to decode.
data Initialization = Initialization
    { initializationLocation :: Maybe (Vector.Vector Int)
    -- ^ The instance's initial position.
    , initializationRotation :: Maybe (Vector.Vector Int8.Int8)
    -- ^ The instance's initial rotation.
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''Initialization)

instance DeepSeq.NFData Initialization where


-- | Gets the 'Initialization' for a given class.
getInitialization :: StrictText.Text -> BinaryBit.BitGet Initialization
getInitialization className = do
    location <- if Set.member className Data.classesWithLocation
        then fmap Just Vector.getIntVector
        else pure Nothing
    rotation <- if Set.member className Data.classesWithRotation
        then fmap Just Vector.getInt8Vector
        else pure Nothing
    pure Initialization { initializationLocation = location, initializationRotation = rotation }


-- | Puts the 'Initialization'. Note that unlike 'getInitialization', this does
-- not require the class name.
putInitialization :: Initialization -> BinaryBit.BitPut ()
putInitialization initialization = do
    case #location initialization of
        Nothing -> pure ()
        Just x -> Vector.putIntVector x
    case #rotation initialization of
        Nothing -> pure ()
        Just x -> Vector.putInt8Vector x
