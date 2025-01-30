{-# LANGUAGE OverloadedStrings #-}

module Settings  where

import           Data.Attoparsec.ByteString       (takeTill)
import qualified Data.Attoparsec.ByteString       as A
import           Data.Attoparsec.ByteString.Char8 (char, decimal)
import qualified Data.ByteString.Char8            as B8
import           Data.Char                        (ord)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Version                     (showVersion)
import           Numeric.Natural                  (Natural)
import           Options.Applicative              (Alternative (many, some, (<|>)),
                                                   Parser, ParserInfo, ReadM,
                                                   argument, auto, eitherReader,
                                                   execParser, footerDoc,
                                                   fullDesc, header, help,
                                                   helper, info, long, metavar,
                                                   option, progDesc, short,
                                                   showDefault, simpleVersioner,
                                                   strOption, value, (<**>))
import           Paths_biblegateway               (version)
import           Prettyprinter                    (Pretty (pretty))
import           System.Directory                 (XdgDirectory (XdgData),
                                                   getXdgDirectory)

type Book = Text
type Chapter = Int
type Verse = Int

defaultVersionName :: Text
defaultVersionName = "RSVCE"

versionStr :: String
versionStr = "biblegateway " <> showVersion version

data Reference = Book Book
               | BookChapter Book Chapter
               | BookChapterRange Book Chapter Chapter
               | BookChapterVerses Book Chapter [Verse]
               | BookChapterVerseRange Book Chapter Verse Verse
               | BookChaptersVerseRange Book Chapter Verse Chapter Verse
 deriving (Show, Eq)

data Settings = Settings
  { baseDir        :: FilePath
  , bibleVersion   :: Text
  , textWidth      :: Natural
  , bibleReference :: [Reference]
  }
 deriving (Show, Eq)

parseCLIArgs :: IO Settings
parseCLIArgs = do
  defaultBaseDir <- getXdgDirectory XdgData "bibles"
  execParser $ opts defaultBaseDir

opts :: FilePath -> ParserInfo Settings
opts defaultBaseDir = info (settingsParser defaultBaseDir <**> helper <**> simpleVersioner versionStr)
   ( fullDesc
  <> header "Bible reader"
  <> progDesc "Read offline holy bible"
  <> footerDoc (Just  (pretty referenceHelp)) )
  where referenceHelp :: String
        referenceHelp =
          "\
           \Reference:\n\
           \    <Book>\n\
           \        Individual book\n\
           \    <Book>:<Chapter>\n\
           \        Individual chapter of a book\n\
           \    <Book>:<Chapter>:<Verse>[,<Verse>]...\n\
           \        Individual verse(s) of a specific chapter of a book\n\
           \    <Book>:<Chapter>-<Chapter>\n\
           \        Range of chapters in a book\n\
           \    <Book>:<Chapter>:<Verse>-<Verse>\n\
           \        Range of verses in a book chapter\n\
           \    <Book>:<Chapter>:<Verse>-<Chapter>:<Verse>\n\
           \        Range of chapters and verses in a book\
           \"

settingsParser :: FilePath -> Parser Settings
settingsParser defaultBaseDir = Settings <$> baseDirParser defaultBaseDir <*> versionNameParser <*> textWidthParser <*> referenceParser

baseDirParser :: FilePath -> Parser FilePath
baseDirParser defaultBaseDir = strOption
     ( long "base-dir"
    <> short 'd'
    <> metavar "DIR"
    <> value defaultBaseDir
    <> help "Use DIR as base directory for bibles. The version name gets appended to it."
    <> showDefault )

versionNameParser :: Parser Text
versionNameParser = strOption
     ( long "version-name"
    <> short 'n'
    <> metavar "NAME"
    <> value defaultVersionName
    <> help "Use NAME as the version name"
    <> showDefault )

textWidthParser :: Parser Natural
textWidthParser = option auto
     ( long "width"
    <> short 'w'
    <> metavar "WIDTH"
    <> value 80
    <> help "Use WIDTH as the rendered text width"
    <> showDefault )

referenceParser :: Parser [Reference]
referenceParser = some $ argument parseReference
     ( metavar "REFERENCE..." )

parseReference :: ReadM Reference
parseReference = eitherReader (A.parseOnly reference . B8.pack)

reference :: A.Parser Reference
reference =
      BookChaptersVerseRange <$> book <*> colon chapter <*> colon verse <*> dash chapter <*> colon verse
  <|> BookChapterVerseRange  <$> book <*> colon chapter <*> colon verse <*> dash verse
  <|> BookChapterVerses      <$> book <*> colon chapter <*> colon verseList
  <|> BookChapterRange       <$> book <*> colon chapter <*> dash chapter
  <|> BookChapter            <$> book <*> colon chapter
  <|> Book                   <$> book
  where verse :: A.Parser Verse
        verse = decimal

        chapter :: A.Parser Chapter
        chapter = decimal

        book :: A.Parser Book
        book = T.pack . B8.unpack . B8.strip <$> takeTill (\c -> c == (fromIntegral . ord $ ':'))

        verseList :: A.Parser [Verse]
        verseList = (:) <$> verse <*> many (char ',' *> verse)

        colon :: A.Parser a -> A.Parser a
        colon p = char ':' *> p

        dash :: A.Parser a -> A.Parser a
        dash p = char '-' *> p

