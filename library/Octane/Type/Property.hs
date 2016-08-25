{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Octane.Type.Property
    ( Property(..)
    , ArrayProperty(..)
    ) where

import Data.Aeson ((.=))
import Data.Function ((&))

import qualified Control.DeepSeq as DeepSeq
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Default.Class as Default
import qualified Data.OverloadedRecords.TH as OverloadedRecords
import qualified GHC.Generics as Generics
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
    = PropertyArray ArrayProperty
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
    deriving (Eq, Generics.Generic, Show)

-- | Stored with the size first, then the value.
instance Binary.Binary Property where
    get = do
        kind <- Binary.get
        case kind of
            _ | kind == arrayProperty -> do
                array <- Binary.get
                pure (PropertyArray array)

            _ | kind == boolProperty -> do
                size <- Binary.get
                value <- Binary.get
                value & BoolProperty size & pure

            _ | kind == byteProperty -> do
                size <- Binary.get
                key <- Binary.get
                if key == "OnlinePlatform_Steam"
                    then ("OnlinePlatform", key) & ByteProperty size & pure
                    else do
                        value <- Binary.get
                        (key, value) & ByteProperty size & pure

            _ | kind == floatProperty -> do
                size <- Binary.get
                value <- case #unpack size of
                    4 -> Binary.get
                    x -> fail ("unknown FloatProperty size " ++ show x)
                value & FloatProperty size & pure

            _ | kind == intProperty -> do
                size <- Binary.get
                value <- case #unpack size of
                    4 -> Binary.get
                    x -> fail ("unknown IntProperty size " ++ show x)
                value & IntProperty size & pure

            _ | kind == nameProperty -> do
                size <- Binary.get
                value <- Binary.get
                value & NameProperty size & pure

            _ | kind == qWordProperty -> do
                size <- Binary.get
                value <- case #unpack size of
                    8 -> Binary.get
                    x -> fail ("unknown QWordProperty size " ++ show x)
                value & QWordProperty size & pure

            _ | kind == strProperty -> do
                size <- Binary.get
                value <- Binary.get
                value & StrProperty size & pure

            _ -> fail ("unknown property type " ++ show (#unpack kind))

    put property =
        case property of
            PropertyArray array -> do
                Binary.put arrayProperty
                Binary.put array

            BoolProperty size value -> do
                Binary.put boolProperty
                Binary.put size
                Binary.put value

            ByteProperty size (key, value) -> do
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

instance DeepSeq.NFData Property where

instance Aeson.ToJSON Property where
    toJSON property = case property of
        PropertyArray array -> Aeson.toJSON array
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


data ArrayProperty = ArrayProperty
    { arrayPropertySize :: Word64.Word64
    , arrayPropertyContent :: List.List (Dictionary.Dictionary Property)
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ArrayProperty)

instance Binary.Binary ArrayProperty where
    get = do
        size <- Binary.get
        content <- Binary.get
        pure (ArrayProperty size content)

    put property = do
        property & #size & Binary.put
        property & #content & Binary.put

instance DeepSeq.NFData ArrayProperty where

instance Aeson.ToJSON ArrayProperty where
    toJSON property = Aeson.object
        [ "Type" .= ("Array" :: Text.Text)
        , "Size" .= #size property
        , "Value" .= #content property
        ]
