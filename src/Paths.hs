-- | Hand-written fallback for the @Paths_bloogle@ module that Cabal normally
-- autogenerates from the .cabal file.
--
-- Under a normal @cabal build@, Cabal generates a @Paths_bloogle@ (with the
-- real package version) into its autogen directory, which is earlier on GHC's
-- module search path and so wins. This file is then NOT compiled at all --
-- note that the filename @Paths.hs@ does not even match the module name, so
-- GHC's module finder never resolves @import Paths_bloogle@ to it.
--
-- It is used only when something compiles this file explicitly, i.e. a build
-- that bypasses Cabal's autogen (feeding source files to GHC by path, an
-- alternate build system, etc.). There it supplies a placeholder version.
-- Nothing uses Cabal's @getDataDir@: the @html/@ assets are embedded in the
-- binary by "General.Embed".
module Paths_bloogle where

import Data.Version.Extra

version :: Version
version = makeVersion [0, 0]
