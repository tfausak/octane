{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module OctaneSpec
  ( spec
  ) where

import qualified Data.Binary as Binary
import qualified Data.Binary.Bits as BinaryBit
import qualified Data.Binary.Bits.Get as BinaryBit
import qualified Data.Binary.Bits.Put as BinaryBit
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import qualified Data.Functor.Identity as Identity
import qualified Data.Map.Strict as Map
import qualified Data.Proxy as Proxy
import qualified Data.Text as StrictText
import qualified Data.Typeable as Typeable
import qualified Data.Version as Version
import qualified Octane
import qualified Test.Tasty.Hspec as Hspec
import qualified Test.Tasty.QuickCheck as QuickCheck

spec :: Hspec.Spec
spec =
  Hspec.parallel $
  Hspec.describe "Octane" $ do
    Hspec.describe "binary" $ do
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Boolean)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.CacheItem)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.CacheProperty)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.ClassItem)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy (Octane.Dictionary ()))
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Float32)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Int8)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Int32)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.KeyFrame)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy (Octane.List ()))
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Mark)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Message)
      pure () -- TODO: binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.OptimizedReplay)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Property)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy (Octane.ArrayProperty ()))
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.BoolProperty)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.ByteProperty)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.FloatProperty)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.IntProperty)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.NameProperty)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.QWordProperty)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.StrProperty)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.RawReplay)
      pure () -- TODO: binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Replay)
      pure () -- TODO: binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.ReplayWithFrames)
      pure () -- TODO: binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.ReplayWithoutFrames)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Stream)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Text)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Word8)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Word16)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Word32)
      binaryRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Word64)
    Hspec.describe "binary bit" $ do
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Boolean)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Float32)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Int8)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Int32)
      pure () -- TODO: binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.PlayStationId)
      pure () -- TODO: binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.SplitscreenId)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.SteamId)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.XboxId)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Text)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Word8)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Word32)
      binaryBitRoundTrip (Proxy.Proxy :: Proxy.Proxy Octane.Word64)
    Hspec.describe "custom binary bit" $ do
      customBinaryBitRoundTrip
        (Proxy.Proxy :: Proxy.Proxy Octane.CompressedWord)
        (\x -> BinaryBit.putBits undefined x)
        (\x -> BinaryBit.getBits (x & #limit & fromIntegral))
      Hspec.it "can round trip Vector Float" $
        QuickCheck.property $ \input -> do
          let put =
                input & floatVectorUnpack & Octane.putFloatVector &
                BinaryBit.runBitPut
          let get =
                Octane.getFloatVector & BinaryBit.runBitGet & fmap FloatVector
          let output = put & Binary.runPut & Binary.runGet get
          let epsilon = 0.0001
          [#x, #y, #z] &
            map
              (\field -> do
                 let expected = input & floatVectorUnpack & field
                 let actual = output & floatVectorUnpack & field
                 let delta = actual - expected
                 Hspec.shouldSatisfy (abs delta) (< epsilon)) &
            Foldable.sequence_
      customBinaryBitRoundTrip
        (Proxy.Proxy :: Proxy.Proxy (Octane.Vector Int))
        (\x -> Octane.putIntVector x)
        (\_ -> Octane.getIntVector)
      customBinaryBitRoundTrip
        (Proxy.Proxy :: Proxy.Proxy (Octane.Vector Octane.Int8))
        (\x -> Octane.putInt8Vector x)
        (\_ -> Octane.getInt8Vector)
    Hspec.describe "Replay" $ do
      let proxy = Proxy.Proxy :: Proxy.Proxy Octane.Replay
      let rid = Identity.runIdentity
      roundTrip
        proxy
        (\x ->
           x & Octane.toOptimizedReplay & rid & Octane.fromOptimizedReplay & rid)

binaryRoundTrip
  :: forall a.
     ( QuickCheck.Arbitrary a
     , Binary.Binary a
     , Eq a
     , Show a
     , Typeable.Typeable a
     )
  => Proxy.Proxy a -> Hspec.SpecWith ()
binaryRoundTrip proxy =
  roundTrip proxy (\x -> x & Binary.encode & Binary.decode)

binaryBitRoundTrip
  :: forall a.
     ( QuickCheck.Arbitrary a
     , BinaryBit.BinaryBit a
     , Eq a
     , Show a
     , Typeable.Typeable a
     )
  => Proxy.Proxy a -> Hspec.SpecWith ()
binaryBitRoundTrip proxy =
  customBinaryBitRoundTrip
    proxy
    (\x -> BinaryBit.putBits undefined x)
    (\_ -> BinaryBit.getBits undefined)

customBinaryBitRoundTrip
  :: forall a.
     (QuickCheck.Arbitrary a, Eq a, Show a, Typeable.Typeable a)
  => Proxy.Proxy a
  -> (a -> BinaryBit.BitPut ())
  -> (a -> BinaryBit.BitGet a)
  -> Hspec.SpecWith ()
customBinaryBitRoundTrip proxy bitPut bitGet =
  roundTrip
    proxy
    (\x -> do
       let put = x & bitPut & BinaryBit.runBitPut
       let get = x & bitGet & BinaryBit.runBitGet
       put & Binary.runPut & Binary.runGet get)

roundTrip
  :: forall a.
     (QuickCheck.Arbitrary a, Eq a, Show a, Typeable.Typeable a)
  => Proxy.Proxy a -> (a -> a) -> Hspec.SpecWith ()
roundTrip proxy f =
  Hspec.it
    ("can round trip " ++ typeName proxy)
    (QuickCheck.property (\x -> Hspec.shouldBe (f x) (x :: a)))

typeName
  :: forall a.
     (Typeable.Typeable a)
  => Proxy.Proxy a -> String
typeName _ = (undefined :: a) & Typeable.typeOf & show

instance QuickCheck.Arbitrary Octane.Boolean where
  arbitrary = Octane.Boolean <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.CacheItem where
  arbitrary =
    Octane.CacheItem <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.CacheProperty where
  arbitrary =
    Octane.CacheProperty <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.ClassItem where
  arbitrary = Octane.ClassItem <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.CompressedWord where
  arbitrary = do
    limit <- QuickCheck.choose (0, 1024)
    value <- QuickCheck.choose (0, limit)
    pure (Octane.CompressedWord limit value)

instance (QuickCheck.Arbitrary a) =>
         QuickCheck.Arbitrary (Octane.Dictionary a) where
  arbitrary =
    Octane.Dictionary <$> QuickCheck.scale (min 2) QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Float32 where
  arbitrary = Octane.Float32 <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Frame where
  arbitrary =
    Octane.Frame <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Initialization where
  arbitrary =
    Octane.Initialization <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Int8 where
  arbitrary = Octane.Int8 <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Int32 where
  arbitrary = Octane.Int32 <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.KeyFrame where
  arbitrary =
    Octane.KeyFrame <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance (QuickCheck.Arbitrary a) =>
         QuickCheck.Arbitrary (Octane.List a) where
  arbitrary = Octane.List <$> QuickCheck.scale (min 2) QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Mark where
  arbitrary = Octane.Mark <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Message where
  arbitrary =
    Octane.Message <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.OptimizedReplay where
  arbitrary =
    Octane.OptimizedReplay <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Property where
  arbitrary =
    QuickCheck.oneof
      [ Octane.PropertyArray <$> QuickCheck.arbitrary
      , Octane.PropertyBool <$> QuickCheck.arbitrary
      , Octane.PropertyByte <$> QuickCheck.arbitrary
      , Octane.PropertyFloat <$> QuickCheck.arbitrary
      , Octane.PropertyInt <$> QuickCheck.arbitrary
      , Octane.PropertyName <$> QuickCheck.arbitrary
      , Octane.PropertyQWord <$> QuickCheck.arbitrary
      , Octane.PropertyStr <$> QuickCheck.arbitrary
      ]

instance (QuickCheck.Arbitrary a) =>
         QuickCheck.Arbitrary (Octane.ArrayProperty a) where
  arbitrary =
    Octane.ArrayProperty <$> QuickCheck.arbitrary <*>
    QuickCheck.scale (min 2) QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.BoolProperty where
  arbitrary =
    Octane.BoolProperty <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.ByteProperty where
  arbitrary =
    Octane.ByteProperty <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.FloatProperty where
  arbitrary = Octane.FloatProperty <$> pure 4 <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.IntProperty where
  arbitrary = Octane.IntProperty <$> pure 4 <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.NameProperty where
  arbitrary =
    Octane.NameProperty <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.QWordProperty where
  arbitrary = Octane.QWordProperty <$> pure 8 <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.StrProperty where
  arbitrary =
    Octane.StrProperty <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.RawReplay where
  arbitrary =
    Octane.newRawReplay <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.RemoteId where
  arbitrary =
    QuickCheck.oneof
      [ Octane.RemotePlayStationId <$> QuickCheck.arbitrary
      , Octane.RemoteSplitscreenId <$> QuickCheck.arbitrary
      , Octane.RemoteSteamId <$> QuickCheck.arbitrary
      , Octane.RemoteXboxId <$> QuickCheck.arbitrary
      ]

instance QuickCheck.Arbitrary Octane.PlayStationId where
  arbitrary =
    Octane.PlayStationId <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.SplitscreenId where
  arbitrary = Octane.SplitscreenId <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.SteamId where
  arbitrary = Octane.SteamId <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.XboxId where
  arbitrary = Octane.XboxId <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Replay where
  arbitrary = do
    let version = Version.makeVersion [868, 12]
    metadata <- QuickCheck.arbitrary
    levels <- QuickCheck.arbitrary
    -- The messages and tick marks must have keys that can be read as
    -- non-negative integers.
    messages <-
      fmap Map.fromList $
      QuickCheck.listOf $ do
        frame <- QuickCheck.arbitrarySizedNatural
        let key = frame & (\x -> x :: Word) & show & StrictText.pack
        value <- QuickCheck.arbitrary
        pure (key, value)
    tickMarks <-
      fmap Map.fromList $
      QuickCheck.listOf $ do
        frame <- QuickCheck.arbitrarySizedNatural
        let key = frame & (\x -> x :: Word) & show & StrictText.pack
        value <- QuickCheck.arbitrary
        pure (key, value)
    packages <- QuickCheck.arbitrary
    -- Each frame has many replications. Each replication has many values. If
    -- we don't limit the number of frames, things get out of hand quickly.
    rawFrames <- QuickCheck.scale (min 10) QuickCheck.arbitrary
    -- The first frame must be a key frame. Only the first frame can be a key
    -- frame.
    let frames =
          rawFrames & zip (True : repeat False) &
          map
            (\(isKeyFrame, frame) -> frame {Octane.frameIsKeyFrame = isKeyFrame})
    pure
      (Octane.Replay version metadata levels messages tickMarks packages frames)

instance QuickCheck.Arbitrary Octane.ReplayWithFrames where
  arbitrary =
    Octane.ReplayWithFrames <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.ReplayWithoutFrames where
  arbitrary =
    Octane.ReplayWithoutFrames <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Replication where
  arbitrary =
    Octane.Replication <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.State where
  arbitrary =
    QuickCheck.elements [Octane.Opening, Octane.Existing, Octane.Closing]

instance QuickCheck.Arbitrary Octane.Stream where
  arbitrary = Octane.Stream <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Text where
  arbitrary = Octane.Text <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Value where
  arbitrary =
    QuickCheck.oneof
      [ Octane.ValueBoolean <$> QuickCheck.arbitrary
      , Octane.ValueByte <$> QuickCheck.arbitrary
      , Octane.ValueCamSettings <$> QuickCheck.arbitrary
      , Octane.ValueDemolish <$> QuickCheck.arbitrary
      , Octane.ValueEnum <$> QuickCheck.arbitrary
      , Octane.ValueExplosion <$> QuickCheck.arbitrary
      , Octane.ValueFlaggedInt <$> QuickCheck.arbitrary
      , Octane.ValueFloat <$> QuickCheck.arbitrary
      , Octane.ValueGameMode <$> QuickCheck.arbitrary
      , Octane.ValueInt <$> QuickCheck.arbitrary
      , Octane.ValueLoadout <$> QuickCheck.arbitrary
      , Octane.ValueLoadoutOnline <$> QuickCheck.arbitrary
      , Octane.ValueLocation <$> QuickCheck.arbitrary
      , Octane.ValueMusicStinger <$> QuickCheck.arbitrary
      , Octane.ValuePickup <$> QuickCheck.arbitrary
      , Octane.ValuePrivateMatchSettings <$> QuickCheck.arbitrary
      , Octane.ValueQWord <$> QuickCheck.arbitrary
      , Octane.ValueRelativeRotation <$> QuickCheck.arbitrary
      , Octane.ValueReservation <$> QuickCheck.arbitrary
      , Octane.ValueRigidBodyState <$> QuickCheck.arbitrary
      , Octane.ValueString <$> QuickCheck.arbitrary
      , Octane.ValueTeamPaint <$> QuickCheck.arbitrary
      , Octane.ValueUniqueId <$> QuickCheck.arbitrary
      ]

instance QuickCheck.Arbitrary Octane.BooleanValue where
  arbitrary = Octane.BooleanValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.ByteValue where
  arbitrary = Octane.ByteValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.CamSettingsValue where
  arbitrary =
    Octane.CamSettingsValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.DemolishValue where
  arbitrary =
    Octane.DemolishValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.EnumValue where
  arbitrary = Octane.EnumValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.ExplosionValue where
  arbitrary =
    Octane.ExplosionValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.FlaggedIntValue where
  arbitrary =
    Octane.FlaggedIntValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.FloatValue where
  arbitrary = Octane.FloatValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.GameModeValue where
  arbitrary = Octane.GameModeValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.IntValue where
  arbitrary = Octane.IntValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.LoadoutOnlineValue where
  arbitrary = Octane.LoadoutOnlineValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.LoadoutValue where
  arbitrary =
    Octane.LoadoutValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.LocationValue where
  arbitrary = Octane.LocationValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.MusicStingerValue where
  arbitrary =
    Octane.MusicStingerValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.PickupValue where
  arbitrary =
    Octane.PickupValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.PrivateMatchSettingsValue where
  arbitrary =
    Octane.PrivateMatchSettingsValue <$> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.QWordValue where
  arbitrary = Octane.QWordValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.RelativeRotationValue where
  arbitrary = Octane.RelativeRotationValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.ReservationValue where
  arbitrary =
    Octane.ReservationValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.RigidBodyStateValue where
  arbitrary =
    Octane.RigidBodyStateValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.StringValue where
  arbitrary = Octane.StringValue <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.TeamPaintValue where
  arbitrary =
    Octane.TeamPaintValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.UniqueIdValue where
  arbitrary =
    Octane.UniqueIdValue <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance (QuickCheck.Arbitrary a) =>
         QuickCheck.Arbitrary (Octane.Vector a) where
  arbitrary =
    Octane.Vector <$> QuickCheck.arbitrary <*> QuickCheck.arbitrary <*>
    QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Word8 where
  arbitrary = Octane.Word8 <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Word16 where
  arbitrary = Octane.Word16 <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Word32 where
  arbitrary = Octane.Word32 <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary Octane.Word64 where
  arbitrary = Octane.Word64 <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary LazyBytes.ByteString where
  arbitrary = LazyBytes.pack <$> QuickCheck.arbitrary

instance QuickCheck.Arbitrary StrictText.Text where
  arbitrary = StrictText.pack <$> QuickCheck.arbitrary

newtype FloatVector = FloatVector
  { floatVectorUnpack :: Octane.Vector Float
  } deriving (Eq, Show)

instance QuickCheck.Arbitrary FloatVector where
  arbitrary = do
    x <- QuickCheck.choose (-1, 1)
    y <- QuickCheck.choose (-1, 1)
    z <- QuickCheck.choose (-1, 1)
    pure (FloatVector (Octane.Vector x y z))
