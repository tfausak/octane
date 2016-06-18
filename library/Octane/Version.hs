module Octane.Version (version) where

import qualified Data.Version as Version
import qualified Paths_octane as This


-- | Octane's version number.
--
-- >>> null (Version.versionBranch version)
-- False
version :: Version.Version
version = This.version
