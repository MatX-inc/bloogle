{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-fields -fno-cse #-}

module Action.CmdLine
  ( CmdLine (..),
    getCmdLine,
    defaultGenerate,
    whenLoud,
    whenNormal,
  )
where

import Data.List.Extra
import Data.Version
import Paths_bloogle (version)
import System.Console.CmdArgs
import System.Environment
import System.IO (hPutStrLn, stderr)

data CmdLine
  = Search
      { color :: Maybe Bool,
        json :: Bool,
        jsonl :: Bool,
        link :: Bool,
        numbers :: Bool,
        info :: Bool,
        database :: FilePath,
        count :: Maybe Int,
        query :: [String],
        repeat_ :: Int,
        compare_ :: [String]
      }
  | Generate
      { database :: FilePath,
        srcdir :: FilePath,
        include :: [String],
        count :: Maybe Int,
        debug :: Bool
      }
  | Server
      { port :: Int,
        database :: FilePath,
        logs :: FilePath,
        local :: Bool,
        links :: Bool,
        scope :: String,
        home :: String,
        host :: String,
        https :: Bool,
        cert :: FilePath,
        key :: FilePath,
        datadir :: Maybe FilePath,
        no_security_headers :: Bool
      }
  | Replay
      { logs :: FilePath,
        database :: FilePath,
        repeat_ :: Int,
        scope :: String
      }
  | Test
      { disable_network_tests :: Bool
      }
  deriving (Data, Typeable, Show)

getCmdLine :: [String] -> IO CmdLine
getCmdLine argv = do
  -- the database is a required positional argument for every command except
  -- `test` (which builds its own throwaway sample database); cmdargs enforces
  -- the requirement via `argPos`, so there is no default location to fill in.
  args1 <- withArgs argv $ cmdArgsRun cmdLineMode

  -- fix up people using Hoogle 4 instructions
  case args1 of
    Generate {..} | "all" `elem` include -> do
      hPutStrLn stderr "Warning: 'all' argument is no longer required, and has been ignored."
      pure $ args1 {include = delete "all" include}
    _ -> pure args1

defaultGenerate :: CmdLine
defaultGenerate = generate

cmdLineMode :: Mode (CmdArgs CmdLine)
cmdLineMode =
  cmdArgsMode $
    modes [search_ &= auto, generate, server, replay, test]
      &= verbosity
      &= program "bloogle"
      &= summary ("Bloogle " ++ showVersion version ++ ", https://bloogle.bluespec.dev/")

search_ :: CmdLine
search_ =
  Search
    { color = def &= name "colour" &= help "Use colored output (requires ANSI terminal)",
      json = def &= name "json" &= help "Get result as JSON",
      jsonl = def &= name "jsonl" &= help "Get result as JSONL (JSON Lines)",
      link = def &= help "Give URL's for each result",
      numbers = def &= help "Give counter for each result",
      info = def &= help "Give extended information about the first n results (set n with --count, default is 1)",
      database = def &= argPos 0 &= typ "DATABASE",
      count = Nothing &= name "n" &= help "Maximum number of results to return (defaults to 10)",
      query = def &= args &= typ "QUERY",
      repeat_ = 1 &= help "Number of times to repeat (for benchmarking)",
      compare_ = def &= help "Type signatures to compare against"
    }
    &= help "Perform a search"

generate :: CmdLine
generate =
  Generate
    { database = def &= argPos 0 &= typ "DATABASE",
      srcdir = def &= argPos 1 &= typ "SRCDIR",
      include = def &= args &= typ "PACKAGE",
      count = Nothing &= name "n" &= help "Maximum number of packages to index (defaults to all)",
      debug = def &= help "Generate debug information"
    }
    &= help "Generate a Bloogle database from a directory of .txt files"

server :: CmdLine
server =
  Server
    { database = def &= argPos 0 &= typ "DATABASE",
      port = 8080 &= typ "INT" &= help "Port number",
      logs = "" &= opt "log.txt" &= typFile &= help "File to log requests to (defaults to stdout)",
      local = False &= help "Allow following file:// links, restricts to 127.0.0.1  Set --host explicitely (including to '*' for any host) to override the localhost-only behaviour",
      scope = def &= help "Default scope to start with",
      links = def &= help "Display extra links",
      home = "http://localhost:8080" &= typ "URL" &= help "Base URL of this server: linked to by the logo and baked into search.xml. Set this to your public URL when deploying.",
      host = "" &= help "Set the host to bind on (e.g., an ip address; '!4' for ipv4-only; '!6' for ipv6-only; default: '*' for any host).",
      https = def &= help "Start an https server (use --cert and --key to specify paths to the .pem files)",
      cert = "cert.pem" &= typFile &= help "Path to the certificate pem file (when running an https server)",
      key = "key.pem" &= typFile &= help "Path to the key pem file (when running an https server)",
      datadir = def &= typDir &= help "Serve the html assets from DIR/html instead of the copies embedded in the binary (they reload without a restart, for front-end development)",
      no_security_headers = False &= help "Don't send CSP security headers"
    }
    &= help "Start a Bloogle server"

replay :: CmdLine
replay =
  Replay
    { database = def &= argPos 0 &= typ "DATABASE",
      logs = "log.txt" &= argPos 1 &= typ "LOGFILE"
    }
    &= help "Replay a log file"

test :: CmdLine
test =
  Test
    { disable_network_tests = False &= help "Disables the use of network tests"
    }
    &= help "Run the test suite"
