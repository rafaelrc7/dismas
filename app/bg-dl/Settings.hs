{-# LANGUAGE OverloadedStrings #-}

module Settings  where

import           Data.Text           (Text)
import           Data.Version        (showVersion)
import           Options.Applicative (Parser, ParserInfo, execParser, fullDesc,
                                      header, help, helper, info, long, metavar,
                                      progDesc, short, showDefault,
                                      simpleVersioner, strOption, switch, value,
                                      (<**>))
import           Paths_biblegateway  (version)
import           System.Directory    (XdgDirectory (XdgData), getXdgDirectory)

defaultLongName :: Text
defaultLongName = "Revised-Standard-Version-Catholic-Edition-RSVCE-Bible"

defaultShortName :: Text
defaultShortName = "RSVCE"

versionStr :: String
versionStr = "biblegateway " <> showVersion version

data Settings = Settings
  { baseDir          :: FilePath
  , versionShortName :: Text
  , versionLongName  :: Text
  , noConfirm        :: Bool
  }
 deriving (Show, Eq)

parseCLIArgs :: IO Settings
parseCLIArgs = do
  defaultBaseDir <- getXdgDirectory XdgData "bibles"
  execParser $ opts defaultBaseDir

opts :: FilePath -> ParserInfo Settings
opts defaultBaseDir = info (settingsParser defaultBaseDir <**> helper <**> simpleVersioner versionStr)
  (  fullDesc
  <> progDesc "Download bibles from biblegateway.com"
  <> header "bg-dl" )

settingsParser :: FilePath -> Parser Settings
settingsParser defaultBaseDir = Settings <$> baseDirParser defaultBaseDir <*> versionShortNameParser <*> versionLongNameParser <*> noConfirmParser

baseDirParser :: FilePath -> Parser FilePath
baseDirParser defaultBaseDir = strOption
     ( long "base-dir"
    <> short 'd'
    <> metavar "BASE DIR"
    <> value defaultBaseDir
    <> help "Use BASE DIR as base directory for bibles download. The short version name gets appended to it."
    <> showDefault )

versionShortNameParser :: Parser Text
versionShortNameParser = strOption
     ( long "short-name"
    <> short 's'
    <> metavar "NAME"
    <> value defaultShortName
    <> help "Use NAME as the short version name, found in the search window url"
    <> showDefault )

versionLongNameParser :: Parser Text
versionLongNameParser = strOption
     ( long "long-name"
    <> short 's'
    <> metavar "NAME"
    <> value defaultLongName
    <> help "Use NAME as the long version name, found in the book list url"
    <> showDefault )

noConfirmParser :: Parser Bool
noConfirmParser = switch
    ( long "no-confirm"
   <> short 'y'
   <> help "Do not prompt for download confirmation" )

