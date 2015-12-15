{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Property where

import Octane.Core
import Octane.Types.Boolean
import Octane.Types.Float32LE
import Octane.Types.Int32LE
import Octane.Types.Int64LE
import Octane.Types.List
import Octane.Types.PCString
import Octane.Types.Table

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
        case (getPCString kind) of
            "ArrayProperty" -> do
                value <- get
                return (ArrayProperty size value)
            "BoolProperty" -> do
                value <- get
                return (BoolProperty size value)
            "ByteProperty" -> do
                key <- get
                value <- get
                return (ByteProperty size (key, value))
            "FloatProperty" -> do
                value <- case size of
                    NewInt64LE 4 -> get
                    _ -> fail ("unknown FloatProperty size " ++ show size)
                return (FloatProperty size value)
            "IntProperty" -> do
                value <- case size of
                    NewInt64LE 4 -> get
                    _ -> fail ("unknown IntProperty size " ++ show size)
                return (IntProperty size value)
            "NameProperty" -> do
                value <- get
                return (NameProperty size value)
            "StrProperty" -> do
                value <- get
                return (StrProperty size value)
            "QWordProperty" -> do
                value <- case size of
                    NewInt64LE 8 -> get
                    _ -> fail ("unknown QWordProperty size " ++ show size)
                return (QWordProperty size value)
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
