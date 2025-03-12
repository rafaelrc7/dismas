{-# LANGUAGE OverloadedStrings #-}

module Settings  where

import qualified Data.Attoparsec.Text as AT
import           Data.Char            (isAlphaNum)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Version         (showVersion)
import           Numeric.Natural      (Natural)
import           Options.Applicative  (Alternative (many, some, (<|>)), Parser,
                                       ParserInfo, ReadM, argument, auto,
                                       eitherReader, execParser, footerDoc,
                                       fullDesc, header, help, helper, info,
                                       long, metavar, option, progDesc, short,
                                       showDefault, simpleVersioner, strOption,
                                       value, (<**>))
import           Paths_dismas         (version)
import           Prettyprinter        (Pretty (pretty))
import           System.Directory     (XdgDirectory (XdgData), getXdgDirectory)

type Book = Text
type Chapter = Int
type Verse = Int

defaultVersionName :: Text
defaultVersionName = "RSVCE"

versionStr :: String
versionStr = "dismas " <> showVersion version

data Query = Reference Reference | Search Search
 deriving (Show, Eq)

data Reference = Book Book
               | BookChapter Book Chapter
               | BookChapterRange Book Chapter Chapter
               | BookChapterVerses Book Chapter [Verse]
               | BookChapterVerseRange Book Chapter Verse Verse
               | BookChaptersVerseRange Book Chapter Verse Chapter Verse
 deriving (Show, Eq)

data Search = SearchWhole Text
            | SearchBook Book Text
            | SearchBookChapter Book Chapter Text
 deriving (Show, Eq)

data Settings = Settings
  { baseDir      :: FilePath
  , bibleVersion :: Text
  , textWidth    :: Natural
  , bibleQueries :: [Query]
  }
 deriving (Show, Eq)

parseCLIArgs :: IO Settings
parseCLIArgs = do
  defaultBaseDir <- getXdgDirectory XdgData "bibles"
  execParser $ opts defaultBaseDir

opts :: FilePath -> ParserInfo Settings
opts defaultBaseDir = info (settingsParser defaultBaseDir <**> helper <**> simpleVersioner versionStr)
   ( fullDesc
  <> header "dismas"
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
           \\n\
           \    /<Search>\n\
           \        All verses that match a pattern\n\
           \    <Book>/<Search>\n\
           \        All verses in a book that match a pattern\n\
           \    <Book>:<Chapter>/<Search>\n\
           \        All verses in a chapter of a book that match a pattern\n\
           \"

settingsParser :: FilePath -> Parser Settings
settingsParser defaultBaseDir = Settings <$> baseDirParser defaultBaseDir <*> versionNameParser <*> textWidthParser <*> queryParser

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

queryParser :: Parser [Query]
queryParser = some $ argument parseQuery
     ( metavar "REFERENCE..." )

parseQuery :: ReadM Query
parseQuery = eitherReader (AT.parseOnly query . T.pack)

query :: AT.Parser Query
query = Reference <$> reference
    <|> Search    <$> search
  where reference :: AT.Parser Reference
        reference =
              BookChaptersVerseRange <$> book <*> colon chapter <*> colon verse <*> dash chapter <*> colon verse <* AT.endOfInput
          <|> BookChapterVerseRange  <$> book <*> colon chapter <*> colon verse <*> dash verse <* AT.endOfInput
          <|> BookChapterVerses      <$> book <*> colon chapter <*> colon verseList <* AT.endOfInput
          <|> BookChapterRange       <$> book <*> colon chapter <*> dash chapter <* AT.endOfInput
          <|> BookChapter            <$> book <*> colon chapter <* AT.endOfInput
          <|> Book                   <$> book <* AT.endOfInput

        search :: AT.Parser Search
        search =
              SearchBookChapter      <$> book <*> colon chapter <*> slash searchText <* AT.endOfInput
          <|> SearchBook             <$> book <*> slash searchText <* AT.endOfInput
          <|> SearchWhole            <$> slash searchText <* AT.endOfInput

        verse :: AT.Parser Verse
        verse = AT.decimal

        chapter :: AT.Parser Chapter
        chapter = AT.decimal

        book :: AT.Parser Book
        book = AT.takeWhile1 isAlphaNum

        verseList :: AT.Parser [Verse]
        verseList = (:) <$> verse <*> many (AT.char ',' *> verse)

        searchText :: AT.Parser Text
        searchText = T.cons <$> AT.satisfy isAlphaNum <*> AT.takeText

        colon :: AT.Parser a -> AT.Parser a
        colon p = AT.char ':' *> p

        dash :: AT.Parser a -> AT.Parser a
        dash p = AT.char '-' *> p

        slash :: AT.Parser a -> AT.Parser a
        slash p = AT.char '/' *> p

