{-# LANGUAGE OverloadedStrings #-}

module Octane.Types.Property where

import Octane.Types.Float32LE (Float32LE)
import Octane.Types.Int32LE (Int32LE)
import Octane.Types.Int64LE (Int64LE)
import Octane.Types.List (List)
import Octane.Types.PCString (PCString)
import Octane.Types.Table (Table)

import qualified Data.Binary as B

data Property
    = ArrayProperty Int64LE (List (Table Property))
    | FloatProperty Int64LE Float32LE
    | IntProperty Int64LE Int32LE
    | NameProperty Int64LE PCString
    | StrProperty Int64LE PCString
    deriving (Show)

instance B.Binary Property where
    get = do
        kind <- B.get
        size <- B.get
        case kind :: PCString of
            "ArrayProperty" -> do
                array <- B.get
                return (ArrayProperty size array)
            "FloatProperty" -> do
                float <- case size of
                    4 -> B.get
                    _ -> fail ("unknown FloatProperty size " ++ show size)
                return (FloatProperty size float)
            "IntProperty" -> do
                int <- case size of
                    4 -> B.get
                    _ -> fail ("unknown IntProperty size " ++ show size)
                return (IntProperty size int)
            "NameProperty" -> do
                name <- B.get
                return (NameProperty size name)
            "StrProperty" -> do
                string <- B.get
                return (StrProperty size string)
            _ -> fail ("unknown property type " ++ show kind)

    put (ArrayProperty size array) = do
        B.put ("ArrayProperty" :: PCString)
        B.put size
        B.put array

    put (FloatProperty size float) = do
        B.put ("FloatProperty" :: PCString)
        B.put size
        B.put float

    put (IntProperty size int) = do
        B.put ("IntProperty" :: PCString)
        B.put size
        B.put int

    put (NameProperty size name) = do
        B.put ("NameProperty" :: PCString)
        B.put size
        B.put name

    put (StrProperty size string) = do
        B.put ("StrProperty" :: PCString)
        B.put size
        B.put string
