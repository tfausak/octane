{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Property (Property(..)) where

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson.Types as Aeson
import qualified Data.Binary as Binary
import Data.Function ((&))
import qualified GHC.Generics as Generics
import qualified Octane.Type.Primitive.Boolean as Boolean
import qualified Octane.Type.Primitive.Dictionary as Dictionary
import qualified Octane.Type.Primitive.Float32 as Float32
import qualified Octane.Type.Primitive.List as List
import qualified Octane.Type.Primitive.Text as Text
import qualified Octane.Type.Primitive.Int32 as Int32
import qualified Octane.Type.Primitive.Word64 as Word64

-- | A metadata property. All properties have a size, but only some actually
-- use it. The value stored in the property can be an array, a boolean, and
-- so on.
data Property
    = ArrayProperty !Word64.Word64
                    !(List.List (Dictionary.Dictionary Property))
    | BoolProperty !Word64.Word64
                   !Boolean.Boolean
    | ByteProperty !Word64.Word64
                   !(Text.Text, Text.Text)
    | FloatProperty !Word64.Word64
                    !Float32.Float32
    | IntProperty !Word64.Word64
                  !Int32.Int32
    | NameProperty !Word64.Word64
                   !Text.Text
    | QWordProperty !Word64.Word64
                    !Word64.Word64
    | StrProperty !Word64.Word64
                  !Text.Text
    deriving (Eq,Generics.Generic,Show)

instance Binary.Binary Property where
    get = do
        kind <- Binary.get
        case kind of
            _ | kind == arrayProperty -> do
                size <- Binary.get
                value <- Binary.get
                value & ArrayProperty size & return
            _ | kind == boolProperty -> do
                size <- Binary.get
                value <- Binary.get
                value & BoolProperty size & return
            _ | kind == byteProperty -> do
                size <- Binary.get
                key <- Binary.get
                if key == "OnlinePlatform_Steam"
                    then ("OnlinePlatform", key) & ByteProperty size & return
                    else do
                        value <- Binary.get
                        (key, value) & ByteProperty size & return
            _ | kind == floatProperty -> do
                size <- Binary.get
                value <- case size of
                    4 -> Binary.get
                    (Word64.Word64 x) ->
                        fail ("unknown FloatProperty size " ++ show x)
                value & FloatProperty size & return
            _ | kind == intProperty -> do
                size <- Binary.get
                value <- case size of
                    4 -> Binary.get
                    (Word64.Word64 x) ->
                        fail ("unknown IntProperty size " ++ show x)
                value & IntProperty size & return
            _ | kind == nameProperty -> do
                size <- Binary.get
                value <- Binary.get
                value & NameProperty size & return
            _ | kind == qWordProperty -> do
                size <- Binary.get
                value <- case size of
                    8 -> Binary.get
                    (Word64.Word64 x) ->
                        fail ("unknown QWordProperty size " ++ show x)
                value & QWordProperty size & return
            _ | kind == strProperty -> do
                size <- Binary.get
                value <- Binary.get
                value & StrProperty size & return
            _ -> fail ("unknown property type " ++ show (Text.unpackText kind))
    put property =
        case property of
            ArrayProperty size value -> do
                Binary.put arrayProperty
                Binary.put size
                Binary.put value
            BoolProperty size value -> do
                Binary.put boolProperty
                Binary.put size
                Binary.put value
            ByteProperty size (key,value) -> do
                Binary.put byteProperty
                Binary.put size
                Binary.put key
                Binary.put value
            FloatProperty size value -> do
                Binary.put floatProperty
                Binary.put size
                Binary.put value
            IntProperty size value -> do
                Binary.put intProperty
                Binary.put size
                Binary.put value
            NameProperty size value -> do
                Binary.put nameProperty
                Binary.put size
                Binary.put value
            QWordProperty size value -> do
                Binary.put qWordProperty
                Binary.put size
                Binary.put value
            StrProperty size value -> do
                Binary.put strProperty
                Binary.put size
                Binary.put value

instance DeepSeq.NFData Property

instance Aeson.ToJSON Property where
    toJSON property =
        case property of
            ArrayProperty _ x -> Aeson.toJSON x
            BoolProperty _ x -> Aeson.toJSON x
            ByteProperty _ (_,x) -> Aeson.toJSON x
            FloatProperty _ x -> Aeson.toJSON x
            IntProperty _ x -> Aeson.toJSON x
            NameProperty _ x -> Aeson.toJSON x
            QWordProperty _ x -> Aeson.toJSON x
            StrProperty _ x -> Aeson.toJSON x

arrayProperty :: Text.Text
arrayProperty = "ArrayProperty"

boolProperty :: Text.Text
boolProperty = "BoolProperty"

byteProperty :: Text.Text
byteProperty = "ByteProperty"

floatProperty :: Text.Text
floatProperty = "FloatProperty"

intProperty :: Text.Text
intProperty = "IntProperty"

nameProperty :: Text.Text
nameProperty = "NameProperty"

qWordProperty :: Text.Text
qWordProperty = "QWordProperty"

strProperty :: Text.Text
strProperty = "StrProperty"
