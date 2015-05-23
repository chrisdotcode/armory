module Main (main) where

import Prelude hiding (putStrLn)

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8 (putStrLn)

import System.Armory.Commands.Internal (load)

main :: IO ()
main = load >>= putStrLn . encodePretty
