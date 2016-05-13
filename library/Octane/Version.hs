module Octane.Version (version) where

import qualified Data.Version as Version
import qualified Paths_octane as This

-- | The current version number.
version :: Version.Version
version = This.version
