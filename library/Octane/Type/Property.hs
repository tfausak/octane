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
    , BoolProperty(..)
    , ByteProperty(..)
    , FloatProperty(..)
    , IntProperty(..)
    , NameProperty(..)
    , QWordProperty(..)
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


data BoolProperty = BoolProperty
    { boolPropertySize :: Word64.Word64
    , boolPropertyContent :: Boolean.Boolean
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''BoolProperty)

instance Binary.Binary BoolProperty where
    get = do
        size <- Binary.get
        content <- Binary.get
        pure (BoolProperty size content)

    put bool = do
        bool & #size & Binary.put
        bool & #content & Binary.put

instance DeepSeq.NFData BoolProperty where

instance Aeson.ToJSON BoolProperty where
    toJSON bool = Aeson.object
        [ "Type" .= ("Bool" :: Text.Text)
        , "Size" .= #size bool
        , "Value" .= #content bool
        ]


data ByteProperty = ByteProperty
    { bytePropertySize :: Word64.Word64
    , bytePropertyKey :: Text.Text
    , bytePropertyValue :: Text.Text
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''ByteProperty)

instance Binary.Binary ByteProperty where
    get = do
        size <- Binary.get
        key <- Binary.get
        if key == "OnlinePlatform_Steam"
            then do
                pure (ByteProperty size "OnlinePlatform" key)
            else do
                value <- Binary.get
                pure (ByteProperty size key value)

    put byte = do
        byte & #size & Binary.put
        byte & #key & Binary.put
        byte & #value & Binary.put

instance DeepSeq.NFData ByteProperty where

instance Aeson.ToJSON ByteProperty where
    toJSON byte = Aeson.object
        [ "Type" .= ("Byte" :: Text.Text)
        , "Size" .= #size byte
        , "Value" .= (#key byte, #value byte)
        ]


data FloatProperty = FloatProperty
    { floatPropertySize :: Word64.Word64
    , floatPropertyContent :: Float32.Float32
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''FloatProperty)

instance Binary.Binary FloatProperty where
    get = do
        size <- Binary.get
        content <- case #unpack size of
            4 -> Binary.get
            x -> fail ("unknown FloatProperty size " ++ show x)
        pure (FloatProperty size content)

    put float = do
        float & #size & Binary.put
        float & #content & Binary.put

instance DeepSeq.NFData FloatProperty where

instance Aeson.ToJSON FloatProperty where
    toJSON float = Aeson.object
        [ "Type" .= ("Float" :: Text.Text)
        , "Size" .= #size float
        , "Value" .= #content float
        ]


data IntProperty = IntProperty
    { intPropertySize :: Word64.Word64
    , intPropertyContent :: Int32.Int32
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''IntProperty)

instance Binary.Binary IntProperty where
    get = do
        size <- Binary.get
        content <- case #unpack size of
            4 -> Binary.get
            x -> fail ("unknown IntProperty size " ++ show x)
        pure (IntProperty size content)

    put int = do
        int & #size & Binary.put
        int & #content & Binary.put

instance DeepSeq.NFData IntProperty where

instance Aeson.ToJSON IntProperty where
    toJSON int = Aeson.object
        [ "Type" .= ("Int" :: Text.Text)
        , "Size" .= #size int
        , "Value" .= #content int
        ]


data NameProperty = NameProperty
    { namePropertySize :: Word64.Word64
    , namePropertyContent :: Text.Text
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''NameProperty)

instance Binary.Binary NameProperty where
    get = do
        size <- Binary.get
        content <- Binary.get
        pure (NameProperty size content)

    put name = do
        name & #size & Binary.put
        name & #content & Binary.put

instance DeepSeq.NFData NameProperty where

instance Aeson.ToJSON NameProperty where
    toJSON name = Aeson.object
        [ "Type" .= ("Name" :: Text.Text)
        , "Size" .= #size name
        , "Value" .= #content name
        ]


data QWordProperty = QWordProperty
    { qWordPropertySize :: Word64.Word64
    , qWordPropertyContent :: Word64.Word64
    } deriving (Eq, Generics.Generic, Show)

$(OverloadedRecords.overloadedRecord Default.def ''QWordProperty)

instance Binary.Binary QWordProperty where
    get = do
        size <- Binary.get
        content <- case #unpack size of
            8 -> Binary.get
            x -> fail ("unknown QWordProperty size " ++ show x)
        pure (QWordProperty size content)

    put qWord = do
        qWord & #size & Binary.put
        qWord & #content & Binary.put

instance DeepSeq.NFData QWordProperty where

instance Aeson.ToJSON QWordProperty where
    toJSON qWord = Aeson.object
        [ "Type" .= ("QWord" :: Text.Text)
        , "Size" .= #size qWord
        , "Value" .= #content qWord
        ]


-- | A metadata property. All properties have a size, but only some actually
-- use it. The value stored in the property can be an array, a boolean, and
-- so on.
data Property
    = PropertyArray ArrayProperty
    | PropertyBool BoolProperty
    | PropertyByte ByteProperty
    | PropertyFloat FloatProperty
    | PropertyInt IntProperty
    | PropertyName NameProperty
    | PropertyQWord QWordProperty
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
                bool <- Binary.get
                pure (PropertyBool bool)

            _ | kind == byteProperty -> do
                byte <- Binary.get
                pure (PropertyByte byte)

            _ | kind == floatProperty -> do
                float <- Binary.get
                pure (PropertyFloat float)

            _ | kind == intProperty -> do
                int <- Binary.get
                pure (PropertyInt int)

            _ | kind == nameProperty -> do
                name <- Binary.get
                pure (PropertyName name)

            _ | kind == qWordProperty -> do
                qWord <- Binary.get
                pure (PropertyQWord qWord)

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

            PropertyBool bool -> do
                Binary.put boolProperty
                Binary.put bool

            PropertyByte byte -> do
                Binary.put byteProperty
                Binary.put byte

            PropertyFloat float -> do
                Binary.put floatProperty
                Binary.put float

            PropertyInt int -> do
                Binary.put intProperty
                Binary.put int

            PropertyName name -> do
                Binary.put nameProperty
                Binary.put name

            PropertyQWord qWord -> do
                Binary.put qWordProperty
                Binary.put qWord

            StrProperty size value -> do
                Binary.put strProperty
                Binary.put size
                Binary.put value

instance DeepSeq.NFData Property where

instance Aeson.ToJSON Property where
    toJSON property = case property of
        PropertyArray array -> Aeson.toJSON array
        PropertyBool bool -> Aeson.toJSON bool
        PropertyByte byte -> Aeson.toJSON byte
        PropertyFloat float -> Aeson.toJSON float
        PropertyInt int -> Aeson.toJSON int
        PropertyName name -> Aeson.toJSON name
        PropertyQWord qWord -> Aeson.toJSON qWord
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

    put array = do
        array & #size & Binary.put
        array & #content & Binary.put

instance DeepSeq.NFData ArrayProperty where

instance Aeson.ToJSON ArrayProperty where
    toJSON array = Aeson.object
        [ "Type" .= ("Array" :: Text.Text)
        , "Size" .= #size array
        , "Value" .= #content array
        ]
