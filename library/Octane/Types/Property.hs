{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Property where

import qualified Data.Binary as Binary
import Flow ((|>))
import Octane.Types.Float32LE
import Octane.Types.Int32LE
import Octane.Types.Int64LE
import Octane.Types.List
import Octane.Types.PCString
import Octane.Types.Table

data Property
    = ArrayProperty Int64LE (List (Table Property))
    | FloatProperty Int64LE Float32LE
    | IntProperty Int64LE Int32LE
    | NameProperty Int64LE PCString
    | StrProperty Int64LE PCString
    deriving (Show)

instance Binary.Binary Property where
    get = do
        kind <- Binary.get
        size <- Binary.get
        case kind :: PCString of
            "ArrayProperty" -> do
                value <- Binary.get
                return (ArrayProperty size value)
            "FloatProperty" -> do
                value <- case size of
                    4 -> Binary.get
                    _ -> fail ("unknown FloatProperty size " ++ show size)
                return (FloatProperty size value)
            "IntProperty" -> do
                value <- case size of
                    4 -> Binary.get
                    _ -> fail ("unknown IntProperty size " ++ show size)
                return (IntProperty size value)
            "NameProperty" -> do
                value <- Binary.get
                return (NameProperty size value)
            "StrProperty" -> do
                value <- Binary.get
                return (StrProperty size value)
            _ -> fail ("unknown property type " ++ show kind)

    put property = case property of
        ArrayProperty size value -> do
            ("ArrayProperty" :: PCString) |> Binary.put
            size |> Binary.put
            value |> Binary.put

        FloatProperty size value -> do
            ("FloatProperty" :: PCString) |> Binary.put
            size |> Binary.put
            value |> Binary.put

        IntProperty size value -> do
            ("IntProperty" :: PCString) |> Binary.put
            size |> Binary.put
            value |> Binary.put

        NameProperty size value -> do
            ("NameProperty" :: PCString) |> Binary.put
            size |> Binary.put
            value |> Binary.put

        StrProperty size value -> do
            ("StrProperty" :: PCString) |> Binary.put
            size |> Binary.put
            value |> Binary.put
