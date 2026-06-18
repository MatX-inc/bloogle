{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}

-- | Module for reading Cabal files.
module Input.Cabal
  ( PkgName,
    Package (..),
    packagePopularity,
    readCabal,
  )
where

import Control.DeepSeq
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Semigroup
import Data.Tuple.Extra
import Distribution.Compat.Lens (toListOf)
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Configuration as PD
import qualified Distribution.PackageDescription.Parsec as PD
import qualified Distribution.Pretty
import qualified Distribution.SPDX as SPDX
import qualified Distribution.Types.BuildInfo.Lens as Lens
import Distribution.Types.PackageName (unPackageName)
import Distribution.Types.Version (versionNumbers)
import Distribution.Utils.ShortText (fromShortText)
import General.Str
import General.Util
import Prelude

---------------------------------------------------------------------
-- DATA TYPE

-- | A representation of a Cabal package.
data Package = Package
  { -- | The Tag information, e.g. (category,Development) (author,Neil Mitchell).
    packageTags :: ![(Str, Str)],
    -- | True if the package provides a library (False if it is only an executable with no API)
    packageLibrary :: !Bool,
    -- | The synposis, grabbed from the top section.
    packageSynopsis :: !Str,
    -- | The version, grabbed from the top section.
    packageVersion :: !Str,
    -- | The list of packages that this package directly depends on.
    packageDepends :: ![PkgName]
  }
  deriving (Show)

instance Semigroup Package where
  Package x1 x2 x3 x4 x5 <> Package y1 y2 y3 y4 y5 =
    Package (x1 ++ y1) (x2 || y2) (one x3 y3) (one x4 y4) (nubOrd $ x5 ++ y5)
    where
      one a b = if strNull a then b else a

instance Monoid Package where
  mempty = Package [] True mempty mempty []
  mappend = (<>)

instance NFData Package where
  rnf (Package a b c d e) = rnf (a, b, c, d, e)

---------------------------------------------------------------------
-- POPULARITY

-- | Given a set of packages, return the popularity of each package, along with any warnings
--   about packages imported but not found.
packagePopularity :: Map.Map PkgName Package -> ([String], Map.Map PkgName Int)
packagePopularity cbl = mp `seq` (errs, mp)
  where
    mp = Map.map length good
    errs =
      [ unPackageName user
          ++ ".cabal: Import of non-existant package "
          ++ unPackageName name
          ++ (if null rest then "" else ", also imported by " ++ show (length rest) ++ " others")
      | (name, user : rest) <- Map.toList bad
      ]
    (good, bad) =
      Map.partitionWithKey (\k _ -> k `Map.member` cbl) $
        Map.fromListWith (++) [(b, [a]) | (a, bs) <- Map.toList cbl, b <- packageDepends bs]

---------------------------------------------------------------------
-- PARSERS

readCabal :: BStr -> Package
readCabal src = case PD.parseGenericPackageDescriptionMaybe src of
  Nothing ->
    Package
      { packageTags = [],
        packageLibrary = False,
        packageSynopsis = mempty,
        packageVersion = strPack "0.0",
        packageDepends = []
      }
  Just gpd -> readCabal' gpd

readCabal' :: PD.GenericPackageDescription -> Package
readCabal' gpd = Package {..}
  where
    pd = PD.flattenPackageDescription gpd
    pkgId = PD.package pd

    packageDepends = nubOrd $ foldMap (map (\(PD.Dependency pkg _ _) -> pkg) . PD.targetBuildDepends) $ toListOf Lens.traverseBuildInfos gpd
    packageVersion = strPack $ intercalate "." $ map show $ versionNumbers $ PD.pkgVersion pkgId
    packageSynopsis = strPack $ fromShortText $ PD.synopsis pd
    packageLibrary = PD.hasPublicLib pd

    unpackLicenseExpression (SPDX.EOr x y) = unpackLicenseExpression x ++ unpackLicenseExpression y
    unpackLicenseExpression x = [x]

    packageLicenses = case PD.license pd of
      SPDX.NONE -> []
      SPDX.License licExpr ->
        map (show . Distribution.Pretty.pretty) $
          unpackLicenseExpression licExpr
    packageCategories =
      filter (not . null) $
        split (`elem` " ,") $
          fromShortText $
            PD.category pd
    packageAuthor = fromShortText $ PD.author pd
    packageMaintainer = fromShortText $ PD.maintainer pd

    packageTags =
      map (both strPack) $
        nubOrd $
          concat
            [ map ("license",) packageLicenses,
              map ("category",) packageCategories,
              map ("author",) (concatMap cleanup [packageAuthor, packageMaintainer])
            ]

    -- split on things like "," "&" "and", then throw away email addresses, replace spaces with "-" and rename
    cleanup =
      filter (/= "")
        . map (intercalate "-" . filter ('@' `notElem`) . words . takeWhile (`notElem` "<("))
        . concatMap (map unwords . split (== "and") . words)
        . split (`elem` ",&")
