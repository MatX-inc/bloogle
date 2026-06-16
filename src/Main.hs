
module Main(main) where

import System.Environment
import System.IO
import Bloogle


main :: IO ()
main = do
    hSetEncoding stdout utf8
    bloogle =<< getArgs
