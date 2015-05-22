module Main (main) where

import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import Prelude hiding (putStrLn, readFile)

import Data.Aeson (decode)
import Data.Aeson.Encode.Pretty (encodePretty)

import Data.ByteString.Lazy (readFile)
import Data.ByteString.Lazy.Char8 (putStrLn)

import System.Armory.Types (Config)

main :: IO ()
main = do
    config <- decode <$> readFile "test/armory.json"
    let config' = encodePretty (fromJust config :: Config)
    putStrLn config'
