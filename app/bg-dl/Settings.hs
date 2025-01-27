module Settings  where

import           Options.Applicative (Parser, ParserInfo, argument, execParser,
                                      fullDesc, header, help, helper, info,
                                      metavar, progDesc, showDefault, str,
                                      value, (<**>))


defaultLongName :: String
defaultLongName = "Revised-Standard-Version-Catholic-Edition-RSVCE-Bible"

defaultShortName :: String
defaultShortName = "RSVCE"

data Settings = Settings
  { baseDir          :: FilePath
  , versionShortName :: String
  , versionLongName  :: String
  }
 deriving (Show, Eq)

parseCLIArgs :: IO Settings
parseCLIArgs = execParser opts

opts :: ParserInfo Settings
opts = info (settingsParser <**> helper)
  (  fullDesc
  <> progDesc "Download bibles from biblegateway.com"
  <> header "bg-dl" )

settingsParser :: Parser Settings
settingsParser = Settings <$> baseDirParser <*> versionShortNameParser <*> versionLongNameParser

-- TODO: Default to XDG_DATA_HOME
baseDirParser :: Parser FilePath
baseDirParser = argument str
     ( metavar "BASE DIR"
    <> help "Base directory for bibles download. The version short name gets appended to it." )

versionShortNameParser :: Parser String
versionShortNameParser = argument str
     ( metavar "VERSION SHORT"
    <> help "Short version name, found in the search window url"
    <> showDefault
    <> value defaultShortName )

versionLongNameParser :: Parser String
versionLongNameParser = argument str
     ( metavar "VERSION LONG"
    <> help "Long version name, found in the book list url"
    <> showDefault
    <> value defaultLongName )

foo :: IO FilePath
foo = return "foo"
