module Octane.Type.Property (Property(..)) where

import Basics

import qualified Data.Aeson as Aeson
import qualified Octane.Type.Boolean as Boolean
import qualified Octane.Type.Dictionary as Dictionary
import qualified Octane.Type.Float32 as Float32
import qualified Octane.Type.Int32 as Int32
import qualified Octane.Type.List as List
import qualified Octane.Type.Text as Text
import qualified Octane.Type.Word64 as Word64


-- | A metadata property. All properties have a size, but only some actually
-- use it. The value stored in the property can be an array, a boolean, and
-- so on.
data Property
    = ArrayProperty
        Word64.Word64
        (List.List (Dictionary.Dictionary Property))
    | BoolProperty
        Word64.Word64
        Boolean.Boolean
    | ByteProperty
        Word64.Word64
        (Text.Text, Text.Text)
    | FloatProperty
        Word64.Word64
        Float32.Float32
    | IntProperty
        Word64.Word64
        Int32.Int32
    | NameProperty
        Word64.Word64
        Text.Text
    | QWordProperty
        Word64.Word64
        Word64.Word64
    | StrProperty
        Word64.Word64
        Text.Text
    deriving (Eq, Generic, Show)

-- | Stored with the size first, then the value.
instance Binary Property where
    get = do
        kind <- get
        case kind of
            _ | kind == arrayProperty -> do
                size <- get
                value <- get
                value & ArrayProperty size & pure

            _ | kind == boolProperty -> do
                size <- get
                value <- get
                value & BoolProperty size & pure

            _ | kind == byteProperty -> do
                size <- get
                key <- get
                if key == "OnlinePlatform_Steam"
                    then ("OnlinePlatform", key) & ByteProperty size & pure
                    else do
                        value <- get
                        (key, value) & ByteProperty size & pure

            _ | kind == floatProperty -> do
                size <- get
                value <- case #unpack size of
                    4 -> get
                    x -> fail ("unknown FloatProperty size " ++ show x)
                value & FloatProperty size & pure

            _ | kind == intProperty -> do
                size <- get
                value <- case #unpack size of
                    4 -> get
                    x -> fail ("unknown IntProperty size " ++ show x)
                value & IntProperty size & pure

            _ | kind == nameProperty -> do
                size <- get
                value <- get
                value & NameProperty size & pure

            _ | kind == qWordProperty -> do
                size <- get
                value <- case #unpack size of
                    8 -> get
                    x -> fail ("unknown QWordProperty size " ++ show x)
                value & QWordProperty size & pure

            _ | kind == strProperty -> do
                size <- get
                value <- get
                value & StrProperty size & pure

            _ -> fail ("unknown property type " ++ show (#unpack kind))

    put property =
        case property of
            ArrayProperty size value -> do
                put arrayProperty
                put size
                put value

            BoolProperty size value -> do
                put boolProperty
                put size
                put value

            ByteProperty size (key, value) -> do
                put byteProperty
                put size
                put key
                put value

            FloatProperty size value -> do
                put floatProperty
                put size
                put value

            IntProperty size value -> do
                put intProperty
                put size
                put value

            NameProperty size value -> do
                put nameProperty
                put size
                put value

            QWordProperty size value -> do
                put qWordProperty
                put size
                put value

            StrProperty size value -> do
                put strProperty
                put size
                put value

instance NFData Property where

instance ToJSON Property where
    toJSON property = case property of
        ArrayProperty size x -> Aeson.object
            [ "Type" .= ("Array" :: Text.Text)
            , "Size" .= size
            , "Value" .= x
            ]
        BoolProperty size x -> Aeson.object
            [ "Type" .= ("Bool" :: Text.Text)
            , "Size" .= size
            , "Value" .= x
            ]
        ByteProperty size x -> Aeson.object
            [ "Type" .= ("Byte" :: Text.Text)
            , "Size" .= size
            , "Value" .= x
            ]
        FloatProperty size x -> Aeson.object
            [ "Type" .= ("Float" :: Text.Text)
            , "Size" .= size
            , "Value" .= x
            ]
        IntProperty size x -> Aeson.object
            [ "Type" .= ("Int" :: Text.Text)
            , "Size" .= size
            , "Value" .= x
            ]
        NameProperty size x -> Aeson.object
            [ "Type" .= ("Name" :: Text.Text)
            , "Size" .= size
            , "Value" .= x
            ]
        QWordProperty size x -> Aeson.object
            [ "Type" .= ("QWord" :: Text.Text)
            , "Size" .= size
            , "Value" .= x
            ]
        StrProperty size x -> Aeson.object
            [ "Type" .= ("Str" :: Text.Text)
            , "Size" .= size
            , "Value" .= x
            ]


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
