{-# LANGUAGE TupleSections, RecordWildCards, ScopedTypeVariables #-}

module Action.Test(actionTest) where

import Query
import Action.CmdLine
import Action.Search
import Action.Server
import Action.Generate
import General.Util
import General.Web
import Input.Item
import Input.Haddock
import System.IO.Extra

import Control.Monad


actionTest :: CmdLine -> IO ()
actionTest Test{..} = withBuffering stdout NoBuffering $ withTempFile $ \sample -> do
    putStrLn "Code tests"
    general_util_test
    general_web_test
    input_haddock_test
    query_test
    action_server_test_
    item_test
    putStrLn ""

    putStrLn "Sample database tests"
    actionGenerate defaultGenerate{database=sample, local_=["misc/sample-data"]}
    action_search_test True sample
    unless disable_network_tests $ action_server_test True sample
    putStrLn ""
