module Octane.Analyzer where

import Data.Function ((&))

import qualified Data.Binary as Binary
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Octane.Parser as Parser

type Point = (Int, Int, Int)
type Points = [Point]
type Frames = [Parser.Frame]

getBallLocations :: Frames -> Points
getBallLocations frames = frames
    & concatMap Parser.frameReplications
    & filter (\ replication ->
        Parser.replicationClassName replication == ballClassName)
    & map Parser.replicationProperties
    & Maybe.mapMaybe (\ properties -> Map.lookup rbsPropertyName properties)
    & Maybe.mapMaybe (\ property -> case property of
        Parser.PRigidBodyState _ location _ _ _ -> Just location
        _ -> Nothing)
    & map (\ (Parser.Vector x y z) -> (x, y, z))

ballClassName :: Text.Text
ballClassName = Text.pack "TAGame.Ball_TA"

rbsPropertyName :: Text.Text
rbsPropertyName = Text.pack "TAGame.RBActor_TA:ReplicatedRBState"

getDistance :: Point -> Point -> Float
getDistance (x1, y1, z1) (x2, y2, z2) = let
    dx = fromIntegral (x2 - x1)
    dy = fromIntegral (y2 - y1)
    dz = fromIntegral (z2 - z1)
    in sqrt (dx ** 2 + dy ** 2 + dz ** 2)

getBallDistances :: Points -> [Float]
getBallDistances points = points
    & zip (drop 1 points)
    & map (\ (p2, p1) -> getDistance p1 p2)

getBallDistance :: Points -> Float
getBallDistance points = points
    & getBallDistances
    & sum

getBallSpeeds :: Points -> [Float]
getBallSpeeds points = points
    & getBallDistances
    & map (\ distance -> distance / 0.04 {- the average delta -})

getHisto :: (a -> Int) -> Int -> Int -> [a] -> (Int, Int, Int)
getHisto fromPoint bottom top points = let
    range = abs (top - bottom)
    third = quot range 3
    values = map fromPoint points
    in foldr
        ((\ value (low, mid, high) ->
            if value > top - third then (low, mid, high + 1)
            else if value < bottom + third then (low + 1, mid, high)
            else (low, mid + 1, high)))
        (0, 0, 0)
        values

getXHisto :: Points -> (Int, Int, Int) -- not sure of the direction... left to right?
getXHisto points = getHisto (\ (x, _, _) -> x) (-4500) 4500 points

getYHisto :: Points -> (Int, Int, Int) -- not sure of the direction... orange to blue?
getYHisto points = getHisto (\ (_, y, _) -> y) (-5200) 5200 points

getZHisto :: Points -> (Int, Int, Int) -- bottom to top
getZHisto points = getHisto (\ (_, _, z) -> z) 0 2000 points

getSpeedHisto :: [Float] -> (Int, Int, Int) -- slow to fast
getSpeedHisto speeds = getHisto round 0 2000 speeds

analyze :: FilePath -> IO ()
analyze file = do
    replay <- Binary.decodeFile file
    let frames = Parser.parseFrames replay

    let ballLocations = getBallLocations frames
    putStr "Number of frames: "
    print (length ballLocations) -- 7702

    let ballDistance = getBallDistance ballLocations
    putStr "Total distance traveled by the ball: "
    print ballDistance -- 323759.94

    let (xs, ys, zs) = unzip3 ballLocations
    let minX = minimum xs
    let maxX = maximum xs
    putStr "(Minimum X value, maximum X value): "
    print (minX, maxX) -- (-4004,4003) [wasteland (-4451,4458)]

    let minY = minimum ys
    let maxY = maximum ys
    putStr "(Minimum Y value, maximum Y value): "
    print (minY, maxY) -- (-5215,5214)

    let minZ = minimum zs
    let maxZ = maximum zs
    putStr "(Minimum Z value, maximum Z value): "
    print (minZ, maxZ) -- (88,1943)

    let xHisto = getXHisto ballLocations
    putStr "(Left, middle, right): "
    print xHisto -- (2241,2237,3224)

    let yHisto = getYHisto ballLocations
    putStr "(Orange, middle, blue): "
    print yHisto -- (3731,1504,2467)

    let zHisto = getZHisto ballLocations
    putStr "(Bottom, middle, top): "
    print zHisto -- (6967,507,228)

    let ballSpeeds = getBallSpeeds ballLocations
    let minSpeed = minimum ballSpeeds
    let maxSpeed = maximum ballSpeeds
    putStr "(Minimum speed, maximum speed): "
    print (minSpeed, maxSpeed) -- (0.0,130894.555)

    let speedHisto = getSpeedHisto ballSpeeds
    putStr "(Slow, medium, fast): "
    print speedHisto -- (2250,3630,1821)
