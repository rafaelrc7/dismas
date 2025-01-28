{-# LANGUAGE OverloadedStrings #-}

module Settings  where

import           Data.Attoparsec.ByteString       (takeTill)
import qualified Data.Attoparsec.ByteString       as A
import           Data.Attoparsec.ByteString.Char8 (char, decimal)
import qualified Data.ByteString.Char8            as B8
import           Data.Char                        (ord)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Options.Applicative              (Alternative (many, (<|>)),
                                                   Parser, ParserInfo, ReadM,
                                                   argument, eitherReader,
                                                   execParser, fullDesc, header,
                                                   help, helper, info, long,
                                                   metavar, progDesc, short,
                                                   showDefault, strOption,
                                                   value, (<**>))
import           System.Directory                 (XdgDirectory (XdgData),
                                                   getXdgDirectory)

type Book = Text
type Chapter = Int
type Verse = Int

defaultVersionName :: Text
defaultVersionName = "RSVCE"

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
  , bibleReference :: Reference
  }
 deriving (Show, Eq)

parseCLIArgs :: IO Settings
parseCLIArgs = do
  defaultBaseDir <- getXdgDirectory XdgData "bibles"
  execParser $ opts defaultBaseDir

opts :: FilePath -> ParserInfo Settings
opts defaultBaseDir = info (settingsParser defaultBaseDir <**> helper)
  (  fullDesc
  <> header "Bible reader"
  <> progDesc "Read offline holy bible" )

settingsParser :: FilePath -> Parser Settings
settingsParser defaultBaseDir = Settings <$> baseDirParser defaultBaseDir <*> versionNameParser <*> referenceParser

baseDirParser :: FilePath -> Parser FilePath
baseDirParser defaultBaseDir = strOption
     ( long "base-dir"
    <> short 'd'
    <> metavar "BASE DIR"
    <> value defaultBaseDir
    <> help "Use BASE DIR as base directory for bibles. The version name gets appended to it."
    <> showDefault )

versionNameParser :: Parser Text
versionNameParser = strOption
     ( long "version-name"
    <> short 'n'
    <> metavar "NAME"
    <> value defaultVersionName
    <> help "Use NAME as the version name"
    <> showDefault )

referenceParser :: Parser Reference
referenceParser = argument parseReference (metavar "REFERENCE")

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

