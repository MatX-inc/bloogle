{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Action.Test (actionTest) where

import Action.CmdLine
import Action.Generate
import Action.Search
import Action.Server
import Control.Monad
import General.Util
import General.Web
import Input.Haddock
import Input.Item
import Query
import System.IO.Extra

actionTest :: CmdLine -> IO ()
actionTest Test {..} = withBuffering stdout NoBuffering $ withTempFile $ \sample -> do
  putStrLn "Code tests"
  general_util_test
  general_web_test
  input_haddock_test
  query_test
  action_server_test_
  item_test
  putStrLn ""

  putStrLn "Sample database tests"
  actionGenerate defaultGenerate {database = sample, local_ = ["misc/sample-data"]}
  action_search_test sample
  unless disable_network_tests $ action_server_test sample
  putStrLn ""
actionTest _ = error "actionTest: expected a Test command"
