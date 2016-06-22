{-# LANGUAGE TemplateHaskell #-}

module Octane.Data.Classes
    ( classes
    , locationClasses
    , rotationClasses
    ) where

import qualified Data.FileEmbed as FileEmbed
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as StrictText
import qualified Octane.Utility.Embed as Embed

-- $
-- :set -XOverloadedStrings


-- | A map from object names to their class names.
--
-- >>> Map.lookup "Archetypes.Ball.Ball_Default" classes
-- Just "TAGame.Ball_TA"
--
-- Note that some object names have been normalized to make lookup easier.
--
-- >>> Map.lookup "Neotokyo_p.TheWorld:PersistentLevel.InMapScoreboard_TA_0@" classes
-- Nothing
-- >>> Map.lookup "TheWorld:PersistentLevel.InMapScoreboard_TA@" classes
-- Just "TAGame.InMapScoreboard_TA"
classes :: Map.Map StrictText.Text StrictText.Text
classes = Embed.decodeMap $(FileEmbed.embedFile "data/classes.json")


-- | A set of classes that have an initial location vector.
--
-- >>> Set.member "TAGame.Ball_TA" locationClasses
-- True
locationClasses :: Set.Set StrictText.Text
locationClasses = Embed.decodeSet $(FileEmbed.embedFile "data/classes-with-location.json")


-- | A set of classes that have an initial rotation vector.
--
-- >>> Set.member "TAGame.Ball_TA" rotationClasses
-- True
rotationClasses :: Set.Set StrictText.Text
rotationClasses = Embed.decodeSet $(FileEmbed.embedFile "data/classes-with-rotation.json")
