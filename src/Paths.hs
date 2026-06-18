-- | Hand-written fallback for the @Paths_bloogle@ module that Cabal normally
-- autogenerates from the .cabal file.
--
-- Under a normal @cabal build@, Cabal generates a @Paths_bloogle@ (with the
-- real package version and the installed data-dir) into its autogen directory,
-- which is earlier on GHC's module search path and so wins. This file is then
-- NOT compiled at all -- note that the filename @Paths.hs@ does not even match
-- the module name, so GHC's module finder never resolves @import Paths_bloogle@
-- to it.
--
-- It is used only when something compiles this file explicitly, i.e. a build
-- that bypasses Cabal's autogen (feeding source files to GHC by path, an
-- alternate build system, etc.). There it supplies a minimal stub: a
-- placeholder version and @getDataDir = "."@ (look for data files, e.g. the
-- @html/@ assets, in the current directory).
module Paths_bloogle where

import Data.Version.Extra

version :: Version
version = makeVersion [0, 0]

getDataDir :: IO FilePath
getDataDir = pure "."
