module Main (main) where

import Bloogle
import System.Environment
import System.IO

main :: IO ()
main = do
  hSetEncoding stdout utf8
  bloogle =<< getArgs
