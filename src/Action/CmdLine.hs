{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-cse #-}

module Action.CmdLine(
    CmdLine(..),
    getCmdLine,
    defaultGenerate,
    whenLoud, whenNormal
    ) where

import Control.Exception.Extra (errorIO)
import Data.List.Extra
import Data.Version
import Paths_bloogle (version)
import System.Console.CmdArgs
import System.Environment

data CmdLine
    = Search
        {color :: Maybe Bool
        ,json :: Bool
        ,jsonl :: Bool
        ,link :: Bool
        ,numbers :: Bool
        ,info :: Bool
        ,database :: FilePath
        ,count :: Maybe Int
        ,query :: [String]
        ,repeat_ :: Int
        ,compare_ :: [String]
        }
    | Generate
        {database :: FilePath
        ,include :: [String]
        ,count :: Maybe Int
        ,local_ :: [FilePath]
        ,haddock :: Maybe FilePath
        ,debug :: Bool
        }
    | Server
        {port :: Int
        ,database :: FilePath
        ,logs :: FilePath
        ,local :: Bool
        ,haddock :: Maybe FilePath
        ,links :: Bool
        ,scope :: String
        ,home :: String
        ,host :: String
        ,https :: Bool
        ,cert :: FilePath
        ,key :: FilePath
        ,datadir :: Maybe FilePath
        ,no_security_headers :: Bool
        }
    | Replay
        {logs :: FilePath
        ,database :: FilePath
        ,repeat_ :: Int
        ,scope :: String
        }
    | Test
        { disable_network_tests  :: Bool
        , database :: FilePath
        }
      deriving (Data,Typeable,Show)

getCmdLine :: [String] -> IO CmdLine
getCmdLine args = do
    args <- withArgs args $ cmdArgsRun cmdLineMode

    -- a database must be specified explicitly; there is no default location
    -- (`test` is exempt: it builds its own throwaway sample database)
    args <- case args of
        Test{} -> pure args
        _ | null (database args) ->
              errorIO "No database specified: pass --database FILE (build one with 'hoogle generate --local=DIR --database=FILE')"
          | otherwise -> pure args

    -- fix up people using Hoogle 4 instructions
    args <- case args of
        Generate{..} | "all" `elem` include -> do
            putStrLn "Warning: 'all' argument is no longer required, and has been ignored."
            pure $ args{include = delete "all" include}
        _ -> pure args

    pure args


defaultGenerate :: CmdLine
defaultGenerate = generate


cmdLineMode = cmdArgsMode $ modes [search_ &= auto,generate,server,replay,test]
    &= verbosity &= program "hoogle"
    &= summary ("Bloogle " ++ showVersion version ++ ", https://bloogle.bluespec.dev/")

search_ = Search
    {color = def &= name "colour" &= help "Use colored output (requires ANSI terminal)"
    ,json = def &= name "json" &= help "Get result as JSON"
    ,jsonl = def &= name "jsonl" &= help "Get result as JSONL (JSON Lines)"
    ,link = def &= help "Give URL's for each result"
    ,numbers = def &= help "Give counter for each result"
    ,info = def &= help "Give extended information about the first n results (set n with --count, default is 1)"
    ,database = def &= typFile &= help "Name of database to use (use .hoo extension)"
    ,count = Nothing &= name "n" &= help "Maximum number of results to return (defaults to 10)"
    ,query = def &= args &= typ "QUERY"
    ,repeat_ = 1 &= help "Number of times to repeat (for benchmarking)"
    ,compare_ = def &= help "Type signatures to compare against"
    } &= help "Perform a search"

generate = Generate
    {include = def &= args &= typ "PACKAGE"
    ,local_ = def &= opt "" &= help "Index local packages and link to local haddock docs"
    ,count = Nothing &= name "n" &= help "Maximum number of packages to index (defaults to all)"
    ,haddock = def &= help "Use local haddocks"
    ,debug = def &= help "Generate debug information"
    } &= help "Generate Bloogle databases"

server = Server
    {port = 8080 &= typ "INT" &= help "Port number"
    ,logs = ""&= opt "log.txt" &= typFile &= help "File to log requests to (defaults to stdout)"
    ,local = False &= help "Allow following file:// links, restricts to 127.0.0.1  Set --host explicitely (including to '*' for any host) to override the localhost-only behaviour"
    ,haddock = def &= help "Serve local haddocks from a specified directory"
    ,scope = def &= help "Default scope to start with"
    ,links = def &= help "Display extra links"
    ,home = "http://localhost:8080" &= typ "URL" &= help "Base URL of this server: linked to by the logo and baked into search.xml. Set this to your public URL when deploying."
    ,host = "" &= help "Set the host to bind on (e.g., an ip address; '!4' for ipv4-only; '!6' for ipv6-only; default: '*' for any host)."
    ,https = def &= help "Start an https server (use --cert and --key to specify paths to the .pem files)"
    ,cert = "cert.pem" &= typFile &= help "Path to the certificate pem file (when running an https server)"
    ,key = "key.pem" &= typFile &= help "Path to the key pem file (when running an https server)"
    ,datadir = def &= help "Override data directory paths"
    ,no_security_headers = False &= help "Don't send CSP security headers"
    } &= help "Start a Bloogle server"

replay = Replay
    {logs = "log.txt" &= args &= typ "FILE"
    } &= help "Replay a log file"

test = Test
    { disable_network_tests = False  &= help "Disables the use of network tests"
    } &= help "Run the test suite"
