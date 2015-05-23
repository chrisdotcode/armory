{-# LANGUAGE FlexibleInstances #-}
module System.Armory.Types
    ( Platform(..)
    , Version
    , Architecture(..)
    , Name
    , InstructionsMap
    , Program(..)
    , Config(..)
    , makeDefaultConfig
    ) where

import Control.Applicative ((<$>), (<*>), empty, pure)
import Data.Version (showVersion)
import System.Info (os)

import Data.Aeson
  ( (.:)
  , (.=)
  , FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , object
  )
import Data.HashMap.Strict (foldrWithKey)
import qualified Data.Map as M (Map, empty, foldrWithKey, insert, singleton)
import Data.Text (Text, pack, unpack)
import Data.Vector (toList)
import System.Directory (doesFileExist)

import Paths_armory (version)

data Platform = Debian
              | Windows
              | Unknown Text
              deriving (Eq, Ord)

instance Read Platform where
    readsPrec _ "debian"  = [(Debian, "")]
    readsPrec _ "windows" = [(Windows, "")]
    readsPrec _ u         = [(Unknown $ pack u, "")]

instance Show Platform where
    show Debian      = "debian"
    show Windows     = "windows"
    show (Unknown u) = unpack u

instance FromJSON Platform where
    parseJSON (String s) = pure $ read $ unpack s
    parseJSON _          = empty

instance ToJSON Platform where
    toJSON = String . pack . show

type Version = Text

data Architecture = Architecture Platform Version
                  | All
                  deriving (Eq, Ord)

instance Read Architecture where
    readsPrec _ "all" = [(All, "")]
    readsPrec _ arch = let (platform, version') = span (/= '@') arch
        in [(Architecture (read platform) (pack version'), "")]

instance Show Architecture where
    show (Architecture platform version') = show platform ++ unpack version'
    show All                             = "all"

instance FromJSON Architecture where
    parseJSON (String s) = pure $ read $ unpack s
    parseJSON _          = empty

instance ToJSON Architecture where
    toJSON = String . pack . show

type Name = Text

type InstructionsMap = M.Map Architecture [Text]

instance (ToJSON v) => ToJSON (M.Map Architecture v) where
  toJSON = object . M.foldrWithKey folder []
    where -- where clause indentation is a little weird here
      folder arch value pairs = pairs ++ [pack (show arch) .= value]

data Program = Program Name InstructionsMap deriving (Eq, Ord, Read, Show)

parseInstructionsMap :: Value -> InstructionsMap
parseInstructionsMap (Object o) = foldrWithKey parseInstructions M.empty o
  where
    stringUnwrapper :: Value -> Text
    stringUnwrapper (String s) = s
    stringUnwrapper v          = error $
        "Expected an instruction as a String, instead received: " ++ show v

    parseInstructions :: Text -> Value -> InstructionsMap -> InstructionsMap
    parseInstructions arch (Array a) =
        M.insert (read $ unpack arch) (map stringUnwrapper $ toList a)
    parseInstructions arch v         = error $
        "Expected an array of instructions for " ++ show arch ++
        ", instead received: " ++ show v
parseInstructionsMap  v         = error $
    "Expected a mapping of architectures to instructions, instead received: "
    ++ show v

instance FromJSON Program where
    parseJSON (Object o) = Program
        <$> o .: "name"
        <*> (parseInstructionsMap <$> o .: "install")
    parseJSON _          = empty

instance ToJSON Program where
    toJSON (Program name instructionsMap) = object
        [ "name" .= name
        , "install" .= instructionsMap
        ]

data Config = Config
    { architecture :: Architecture
    , installer    :: Text
    , uninstaller  :: Text
    , programs     :: [Program]
    } deriving (Eq, Ord, Read, Show)

-- Can't use data-default due to overlapping instance of `Data.Default a => Data.Default (IO a)` and `IO Config` here.
makeDefaultConfig :: IO Config
makeDefaultConfig = do
    archGuess <- do
        debian <- doesFileExist "/etc/debian_version"
        if debian
          then return Debian
          else
              if os == "mingw32"
                then return Windows
                else return $ Unknown $ pack os

    (installer', uninstaller') <- case archGuess of
        Debian     -> return ("apt-get install -y", "apt-get remove -y --purge")
        Windows    -> return ("choco install -y", "choco uninstall -y")
        Unknown _ -> do
            putStrLn "Unrecognized operating system."
            putStr "What command do you install programs with? (e.g. `apt-get install -y` on Debian) "
            installer'' <- pack <$> getLine
            putStr "What command do you remove programs with? (e.g. `apt-get remove -y --purge` on Debian) "
            uninstaller'' <- pack <$> getLine
            return (installer'', uninstaller'')

    let arch = Architecture archGuess ""

    return $ Config
        arch
        installer'
        uninstaller'
        [ Program
            (pack $ "armory@" ++ showVersion version)
            (M.singleton arch ["armory install armory"])
        ]

instance FromJSON Config where
    parseJSON (Object o) = Config
        <$> o .: "architecture"
        <*> o .: "installer"
        <*> o .: "uninstaller"
        <*> o .: "programs"
    parseJSON _          = empty

instance ToJSON Config where
    toJSON (Config a i u p) = object
        [ "architecture" .= a
        , "installer"    .= i
        , "uninstaller"  .= u
        , "programs"     .= p
        ]
