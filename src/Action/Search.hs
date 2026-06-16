{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Action.Search
  ( actionSearch,
    withSearch,
    search,
    targetInfo,
    targetResultDisplay,
    action_search_test,
  )
where

import Action.CmdLine
import Control.DeepSeq
import Control.Exception.Extra
import Control.Monad.Extra
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Functor.Identity
import Data.List.Extra
import Data.Maybe
import qualified Data.Set as Set
import General.Store
import General.Util
import Input.Item
import Output.Items
import Output.Names
import Output.Tags
import Output.Types
import Query
import Safe
import System.Console.ANSI
  ( Color (Yellow),
    ColorIntensity (Dull, Vivid),
    ConsoleLayer (Foreground),
    SGR (SetColor),
    hSupportsANSI,
    hyperlinkCode,
    setSGRCode,
  )
import System.Directory
import System.IO (stdout)
import Text.Blaze.Renderer.Utf8

-- -- generate all
-- @tagsoup -- generate tagsoup
-- @tagsoup filter -- search the tagsoup package
-- filter -- search all

actionSearch :: CmdLine -> IO ()
actionSearch Search {..} = replicateM_ repeat_ $ -- deliberately reopen the database each time
  withSearch database $ \store ->
    if null compare_
      then do
        count' <- pure $ fromMaybe 10 count
        (q, res) <- pure $ search store $ parseQuery $ unwords query
        whenLoud $ putStrLn $ "Query: " ++ unescapeHTML (LBS.unpack $ renderMarkup $ renderQuery q)
        color' <- case color of
          Just b -> pure b
          Nothing -> hSupportsANSI stdout
        let (shown, hidden) = splitAt count' $ nubOrd $ map (targetResultDisplay link color' q) res
        if null res
          then
            putStrLn "No results found"
          else
            if info
              then do
                mapM_ (putStr . targetInfo color' q) $
                  ( case count of
                      Just c -> take c
                      Nothing -> singleton . headErr
                  )
                    res
              else do
                if
                  | json -> LBS.putStrLn $ JSON.encode $ maybe id take count $ map unHTMLtargetItem res
                  | jsonl -> mapM_ (LBS.putStrLn . JSON.encode) $ maybe id take count $ map unHTMLtargetItem res
                  | otherwise -> putStr $ unlines $ if numbers then addCounter shown else shown
                when (hidden /= [] && not json) $ do
                  whenNormal $ putStrLn $ "-- plus more results not shown, pass --count=" ++ show (count' + 10) ++ " to see more"
      else do
        let parseType x = case parseQuery x of
              [QueryType t] -> (pretty t, hseToSig t)
              _ -> error $ "Expected a type signature, got: " ++ x
        putStr $ unlines $ searchFingerprintsDebug store (parseType $ unwords query) (map parseType compare_)
actionSearch _ = error "actionSearch: expected a Search command"

-- | Returns the details printed out when hoogle --info is called
targetInfo :: Bool -> [Query] -> Target -> String
targetInfo color qs Target {..} =
  unlines $
    [unHTML . (if color then ansiHighlight qs else id) $ targetItem]
      ++ [unwords packageModule | not $ null packageModule]
      ++ [unHTML targetDocs]
  where
    packageModule = map fst $ catMaybes [targetPackage, targetModule]

-- | Returns the Target formatted as an item to display in the results
-- | Bool arguments decide whether links and colors are shown
targetResultDisplay :: Bool -> Bool -> [Query] -> Target -> String
targetResultDisplay link color qs Target {..} =
  unHTML $
    unwords $
      map fst (maybeToList targetModule)
        ++ [if color then highlightFull targetItem else targetItem]
        ++ ["-- " ++ targetURL | link]
  where
    highlightFull = hyperlinkCode targetURL . ansiHighlight qs

ansiHighlight :: [Query] -> String -> String
ansiHighlight = highlightItem id id ((dull ++) . (++ rst)) ((bold ++) . (++ rst))
  where
    dull = setSGRCode [SetColor Foreground Dull Yellow]
    bold = setSGRCode [SetColor Foreground Vivid Yellow]
    rst = setSGRCode []

unHTMLtargetItem :: Target -> Target
unHTMLtargetItem target = target {targetItem = unHTML $ targetItem target}

addCounter :: [String] -> [String]
addCounter = zipWithFrom (\i x -> show i ++ ") " ++ x) (1 :: Int)

withSearch :: (NFData a) => FilePath -> (StoreRead -> IO a) -> IO a
withSearch database act = do
  unlessM (doesFileExist database) $ do
    exitFail $
      "Error, database does not exist (run 'hoogle generate' first)\n"
        ++ "    Filename: "
        ++ database
  storeReadFile database act

search :: StoreRead -> [Query] -> ([Query], [Target])
search store qs = runIdentity $ do
  (qs', exact, filt, list') <- pure $ applyTags store qs
  is <- case (filter isQueryName qs', filter isQueryType qs') of
    ([], []) -> pure list'
    ([], t : _) -> pure $ searchTypes store $ hseToSig $ fromQueryType t
    (xs, []) -> pure $ searchNames store exact $ map fromQueryName xs
    (xs, t : _) -> do
      nam <- pure $ Set.fromList $ searchNames store exact $ map fromQueryName xs
      pure $ filter (`Set.member` nam) $ searchTypes store $ hseToSig $ fromQueryType t
  let look = lookupItem store
  pure (qs', map look $ filter filt is)

action_search_test :: FilePath -> IO ()
action_search_test database = testing "Action.Search.search" $ withSearch database $ \store -> do
  let a ==$ f = do
        res <- pure $ snd $ search store (parseQuery a)
        case res of
          Target {..} : _ | f targetURL -> putChar '.'
          _ -> errorIO $ "Searching for: " ++ show a ++ "\nGot: " ++ show (take 1 res)
  let a === b = a ==$ (== b)

  "__prefix__" === "http://henry.com?too_long"
  "__suffix__" === "http://henry.com?too_long"
  "__infix__" === "http://henry.com?too_long"
  "Wife" === "http://eghmitchell.com/Mitchell.html#a_wife"
  completionTags store `testEq` ["set:all", "set:sample-data", "package:emily", "package:henry"]
