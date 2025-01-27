{-# LANGUAGE OverloadedStrings #-}

module Settings  where

import           Data.Attoparsec.ByteString       (takeTill)
import qualified Data.Attoparsec.ByteString       as A
import           Data.Attoparsec.ByteString.Char8 (char, decimal)
import           Data.ByteString.Char8            (pack, strip, unpack)
import           Data.Char                        (ord)
import           Options.Applicative              (Alternative (many, (<|>)),
                                                   Parser, ParserInfo, ReadM,
                                                   argument, eitherReader,
                                                   execParser, fullDesc, header,
                                                   helper, info, metavar,
                                                   progDesc, str, (<**>))

type Book = String
type Chapter = Int
type Verse = Int

data Reference = Book Book
               | BookChapter Book Chapter
               | BookChapterRange Book Chapter Chapter
               | BookChapterVerses Book Chapter [Verse]
               | BookChapterVerseRange Book Chapter Verse Verse
               | BookChaptersVerseRange Book Chapter Verse Chapter Verse
 deriving (Show, Eq)


data Settings = Settings
  { biblePath :: FilePath
  , selector  :: Reference
  }
 deriving (Show, Eq)

parseCLIArgs :: IO Settings
parseCLIArgs = execParser opts

opts :: ParserInfo Settings
opts = info (settingsParser <**> helper)
  (  fullDesc
  <> header "Bible reader"
  <> progDesc "Read offline holy bible" )

settingsParser :: Parser Settings
settingsParser = Settings <$> biblePathParser <*> referenceParser

biblePathParser :: Parser FilePath
biblePathParser = argument str (metavar "BIBLE_PATH")

referenceParser :: Parser Reference
referenceParser = argument parseReference (metavar "REFERENCE")

parseReference :: ReadM Reference
parseReference = eitherReader (A.parseOnly reference . pack)

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
        book = unpack . strip <$> takeTill (\c -> c == (fromIntegral . ord $ ':'))

        verseList :: A.Parser [Verse]
        verseList = (:) <$> verse <*> many (char ',' *> verse)

        colon :: A.Parser a -> A.Parser a
        colon p = char ':' *> p

        dash :: A.Parser a -> A.Parser a
        dash p = char '-' *> p

