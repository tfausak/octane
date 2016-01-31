{-# LANGUAGE OverloadedStrings #-}

module Octane.Type.Property (Property(..)) where

import Octane.Core
import Octane.Type.List
import Octane.Type.PCString
import Octane.Type.Primitive.Boolean
import Octane.Type.Primitive.Float32LE
import Octane.Type.Primitive.Int32LE
import Octane.Type.Primitive.Int64LE
import Octane.Type.Table

data Property
    = ArrayProperty Int64LE (List (Table Property))
    | BoolProperty Int64LE Boolean
    | ByteProperty Int64LE (PCString, PCString)
    | FloatProperty Int64LE Float32LE
    | IntProperty Int64LE Int32LE
    | NameProperty Int64LE PCString
    | QWordProperty Int64LE Int64LE
    | StrProperty Int64LE PCString
    deriving (Show)

instance Binary Property where
    get = do
        kind <- get
        size <- get
        case getPCString kind of
            "ArrayProperty" -> do
                value <- get
                ArrayProperty size value & return
            "BoolProperty" -> do
                value <- get
                BoolProperty size value & return
            "ByteProperty" -> do
                key <- get
                value <- get
                ByteProperty size (key, value) & return
            "FloatProperty" -> do
                value <- case size of
                    NewInt64LE 4 -> get
                    _ -> fail ("unknown FloatProperty size " ++ show size)
                FloatProperty size value & return
            "IntProperty" -> do
                value <- case size of
                    NewInt64LE 4 -> get
                    _ -> fail ("unknown IntProperty size " ++ show size)
                IntProperty size value & return
            "NameProperty" -> do
                value <- get
                NameProperty size value & return
            "StrProperty" -> do
                value <- get
                StrProperty size value & return
            "QWordProperty" -> do
                value <- case size of
                    NewInt64LE 8 -> get
                    _ -> fail ("unknown QWordProperty size " ++ show size)
                QWordProperty size value & return
            _ -> fail ("unknown property type " ++ show kind)

    put property = case property of
        ArrayProperty size value -> do
            "ArrayProperty" & NewPCString & put
            size & put
            value & put
        BoolProperty size value -> do
            "BoolProperty" & NewPCString & put
            size & put
            value & put
        ByteProperty size (key, value) -> do
            "ByteProperty" & NewPCString & put
            size & put
            key & put
            value & put
        FloatProperty size value -> do
            "FloatProperty" & NewPCString & put
            size & put
            value & put
        IntProperty size value -> do
            "IntProperty" & NewPCString & put
            size & put
            value & put
        NameProperty size value -> do
            "NameProperty" & NewPCString & put
            size & put
            value & put
        QWordProperty size value -> do
            "QWordProperty" & NewPCString & put
            size & put
            value & put
        StrProperty size value -> do
            "StrProperty" & NewPCString & put
            size & put
            value & put
