-- | High level Bloogle API
module Bloogle
  ( Database,
    withDatabase,
    searchDatabase,
    Target (..),
    URL,
    bloogle,
    targetInfo,
    targetResultDisplay,
  )
where

import Action.CmdLine
import Action.Generate
import Action.Search
import Action.Server
import Action.Test
import Control.DeepSeq (NFData)
import General.Store
import General.Util
import Input.Item
import Query

-- | Database containing Bloogle search data.
newtype Database = Database StoreRead

-- | Load a database from a file.
withDatabase :: (NFData a) => FilePath -> (Database -> IO a) -> IO a
withDatabase file act = storeReadFile file $ act . Database

-- | Search a database, given a query string, produces a list of results.
searchDatabase :: Database -> String -> [Target]
searchDatabase (Database db) q = snd $ search db $ parseQuery q

-- | Run a command line Bloogle operation.
bloogle :: [String] -> IO ()
bloogle args = do
  args' <- getCmdLine args
  case args' of
    Search {} -> actionSearch args'
    Generate {} -> actionGenerate args'
    Server {} -> actionServer args'
    Test {} -> actionTest args'
    Replay {} -> actionReplay args'
