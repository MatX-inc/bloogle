{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Action.Generate (actionGenerate) where

import Action.CmdLine
import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad.Extra
import Data.IORef
import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Set as Set
import Data.Tuple.Extra
import Distribution.Package (mkPackageName, unPackageName)
import General.Conduit
import General.Store
import General.Str
import General.Timing
import General.Util
import Input.Cabal
import Input.Haddock
import Input.Item
import Input.Reorder
import Output.Items
import Output.Names
import Output.Tags
import Output.Types
import System.Console.CmdArgs.Verbosity
import System.Directory.Extra
import System.FilePath
import System.IO.Extra
import Prelude

{-

data GenList
    = GenList_Package String -- a literally named package
    | GenList_GhcPkg String -- command to run, or "" for @ghc-pkg list@
    | GenList_Stackage String -- URL of stackage file, defaults to @http://www.stackage.org/lts/cabal.config@
    | GenList_Dependencies String -- dependencies in a named .cabal file
    | GenList_Sort String -- URL of file to sort by, defaults to @http://packdeps.haskellers.com/reverse@

data GenTags
    = GenTags_GhcPkg String -- command to run, or "" for @ghc-pkg dump@
    | GenTags_Diff FilePath -- a diff to apply to previous metadata
    | GenTags_Tarball String -- tarball of Cabal files, defaults to http://hackage.haskell.org/packages/index.tar.gz
    | GetTags_Cabal FilePath -- tarball to get tag information from

data GenData
    = GenData_File FilePath -- a file containing package data
    | GenData_Tarball String -- URL where a tarball of data files resides

\* `hoogle generate` - generate for all things in Stackage based on Hackage information.
\* `hoogle generate --source=file1.txt --source=local --source=stackage --source=hackage --source=tarball.tar.gz`

Which files you want to index. Currently the list on stackage, could be those locally installed, those in a .cabal file etc. A `--list` flag, defaults to `stackage=url`. Can also be `ghc-pkg`, `ghc-pkg=user` `ghc-pkg=global`. `name=p1`.

Extra metadata you want to apply. Could be a file. `+shake author:Neil-Mitchell`, `-shake author:Neil-Mitchel`. Can be sucked out of .cabal files. A `--tags` flag, defaults to `tarball=url` and `diff=renamings.txt`.

Where the haddock files are. Defaults to `tarball=hackage-url`. Can also be `file=p1.txt`. Use `--data` flag.

Defaults to: `hoogle generate --list=ghc-pkg --list=constrain=stackage-url`.

Three pieces of data:

\* Which packages to index, in order.
\* Metadata.

generate :: Maybe Int -> [GenList] -> [GenTags] -> [GenData] -> IO ()
-- how often to redownload, where to put the files

generate :: FilePath -> [(String, [(String, String)])] -> [(String, LBS.ByteString)] -> IO ()
generate output metadata = ...
-}

-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

readHaskellDirs :: Timing -> [FilePath] -> IO (Map.Map PkgName Package, Set.Set PkgName, ConduitT () (PkgName, URL, LBStr) IO ())
readHaskellDirs _timing dirs = do
  files <- concatMapM listFilesRecursive dirs
  -- We reverse/sort the list because of #206
  -- Two identical package names with different versions might be foo-2.0 and foo-1.0
  -- We never distinguish on versions, so they are considered equal when reordering
  -- So put 2.0 first in the list and rely on stable sorting. A bit of a hack.
  let order a = second Down $ parseTrailingVersion a
  let packages = map (mkPackageName . takeBaseName &&& id) $ sortOn (map order . splitDirectories) $ filter ((==) ".txt" . takeExtension) files
  cabals <- mapM parseCabal $ filter ((==) ".cabal" . takeExtension) files
  let source = forM_ packages $ \(name, file) -> do
        src <- liftIO $ bstrReadFile file
        dir <- liftIO $ canonicalizePath $ takeDirectory file
        let url = "file://" ++ ['/' | not $ "/" `isPrefixOf` dir] ++ replace "\\" "/" dir ++ "/"
        when (isJust $ bstrSplitInfix (bstrPack "@package " <> bstrPack (unPackageName name)) src) $
          yield (name, url, lbstrFromChunks [src])
  pure
    ( Map.union
        (Map.fromList cabals)
        (Map.fromListWith (<>) $ map generateBarePackage packages),
      Set.fromList $ map fst packages,
      source
    )
  where
    parseCabal fp = do
      src <- bstrReadFile fp
      let pkg = readCabal src
      pure (mkPackageName $ takeBaseName fp, pkg)

    generateBarePackage (name, file) =
      (name, mempty {packageTags = (strPack "set", strPack "all") : sets})
      where
        sets = map setFromDir $ filter (`isPrefixOf` file) dirs
        setFromDir dir = (strPack "set", strPack $ takeFileName $ dropTrailingPathSeparator dir)

actionGenerate :: CmdLine -> IO ()
actionGenerate _g@Generate {..} = withTiming' $ \timing -> do
  whenLoud $ putStrLn "Starting generate"
  createDirectoryIfMissing True $ takeDirectory database
  whenLoud $ putStrLn $ "Generating files to " ++ takeDirectory database

  (cbl, want, source) <- readHaskellDirs timing [srcdir]
  (cblErrs, popularity) <- evaluate $ packagePopularity cbl
  cbl' <- evaluate $ Map.map (\p -> p {packageDepends = []}) cbl -- clear the memory, since the information is no longer used
  _ <- evaluate popularity

  want' <- pure $ if include /= [] then Set.fromList $ map mkPackageName include else want
  want'' <- pure $ case count of Nothing -> want'; Just count' -> Set.fromList $ take count' $ Set.toList want'

  (stats, _) <- storeWriteFile database $ \store -> do
    xs <- do
      -- Warnings go to stderr as they are found, unless --quiet.
      let warning msg = whenNormal $ hPutStrLn stderr msg
      mapM_ warning cblErrs

      itemWarn <- newIORef (0 :: Integer)
      let itemWarning msg = do modifyIORef itemWarn succ; warning msg

      let consume :: ConduitM (Int, (PkgName, URL, LBStr)) (Maybe Target, [Item]) IO ()
          consume = awaitForever $ \(i, (unPackageName -> pkg, url, body)) -> do
            timedOverwrite timing ("[" ++ show i ++ "/" ++ show (Set.size want'') ++ "] " ++ pkg) $
              parseHoogle (\msg -> itemWarning $ pkg ++ ":" ++ msg) url body

      writeItems store $ \items -> do
        xs <-
          runConduit $
            source
              .| filterC (flip Set.member want'' . fst3)
              .| void
                ( (|$|)
                    (zipFromC 1 .| consume)
                    ( do
                        seen <- fmap Set.fromList $ mapMC (evaluate . force . fst3) .| sinkList
                        let missing =
                              [ x
                              | x <- Set.toList $ want'' `Set.difference` seen,
                                fmap packageLibrary (Map.lookup x cbl') /= Just False
                              ]
                        liftIO $ whenLoud $ putStrLn "" -- end the progress line
                        liftIO $ whenNormal $ when (missing /= []) $ do
                          hPutStrLn stderr $ "Packages missing documentation: " ++ unwords (sortOn lower $ map unPackageName missing)
                        liftIO $
                          when (Set.null seen) $
                            exitFail "No packages were found in the source directory, aborting"
                        -- synthesise things for Cabal packages that are not documented
                        forM_ (Map.toList cbl') $ \(name, Package {..}) -> when (name `Set.notMember` seen) $ do
                          let ret prefix = yield $ fakePackage name $ prefix ++ trim (strUnpack packageSynopsis)
                          if name `Set.member` want''
                            then
                              ( if packageLibrary
                                  then ret "Documentation not found, so not searched.\n"
                                  else ret "Executable only. "
                              )
                            else
                              if null include
                                then
                                  ret "Not on Stackage, so not searched.\n"
                                else
                                  pure ()
                    )
                )
              .| pipelineC 10 (items .| sinkList)

        itemWarn' <- readIORef itemWarn
        when (itemWarn' > 0) $
          whenNormal $
            hPutStrLn stderr $
              "Found " ++ show itemWarn' ++ " warning" ++ ['s' | itemWarn' /= 1] ++ " when processing items"
        pure [(a, b) | (a, bs) <- xs, b <- bs]

    itemsMemory <- getStatsCurrentLiveBytes
    xs' <- timed timing "Reordering items" $ pure $! reorderItems (\s -> maybe 1 negate $ Map.lookup s popularity) xs
    timed timing "Writing tags" $ writeTags store (`Set.member` want'') (\x -> maybe [] (map (both strUnpack) . packageTags) $ Map.lookup x cbl') xs'
    timed timing "Writing names" $ writeNames store xs'
    timed timing "Writing types" $ writeTypes store (if debug then Just $ dropExtension database else Nothing) xs'

    whenLoud $ do
      whenJustM getStatsDebug print
      whenJustM getStatsPeakAllocBytes $ \x ->
        putStrLn $ "Peak of " ++ x ++ ", " ++ fromMaybe "unknown" itemsMemory ++ " for items"

  when debug $
    writeFile (database `replaceExtension` "store") $
      unlines stats
  where
    -- Progress and timings are only printed under --verbose. By default the
    -- only output is warnings (on stderr), so a build system's action output
    -- stays quiet unless something needs attention.
    withTiming' act = do
      verbose <- isLoud
      withTiming verbose (if debug then Just $ replaceExtension database "timing" else Nothing) act
actionGenerate _ = error "actionGenerate: expected Generate"
