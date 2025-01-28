{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative      (Alternative (many), (<|>))
import           Control.Concurrent.Async (async, wait)
import           Control.Monad            (forM, forM_, unless, void)
import           Data.Char                (isSpace)
import           Data.Function            (on)
import           Data.List                (sortBy)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Read           as T
import           Settings                 (Settings (baseDir, noConfirm, versionLongName, versionShortName),
                                           parseCLIArgs)
import           System.Directory         (createDirectoryIfMissing)
import           System.IO                (hPutStrLn, stderr)
import           Text.HTML.Scalpel        (Scraper, atDepth, attr, chroot,
                                           chroots, hasClass, html, inSerial,
                                           scrapeURL, seekNext, stepNext, text,
                                           textSelector, (//), (@:), (@=~))
import           Text.Regex.TDFA

type Verse = Text
type Chapter = [Verse]

data Error = InvalidVerseSpan
           | InternalError
  deriving (Show)

main :: IO ()
main = do
  settings <- parseCLIArgs
  let dir = baseDir settings <> "/" ++ T.unpack (versionShortName settings)
  T.putStrLn $ "Will download the '" <> versionShortName settings <> "' from the biblegateway site to '" <> T.pack dir <> "'"
  unless (noConfirm settings) $ do
    putStrLn "Press ENTER to continue or ^C to cancel..."
    void getLine
  downloadBible dir (versionShortName settings) (versionLongName settings)

----

scrapeBibleBooks :: Text -> IO (Either Error [(Text, Int)])
scrapeBibleBooks version = do
    ret <- scrapeURL (T.unpack link) booksScraper
    case ret of
      Nothing -> return $ Left InternalError
      Just c  -> return c
  where link = "https://www.biblegateway.com/versions/" <> version <> "/#booklist"

        booksScraper :: Scraper Text (Either Error [(Text, Int)])
        booksScraper = sequence <$> chroot ("table" @: [hasClass "chapterlinks"]) (inSerial $ many $
                         seekNext $ chroot ("tr" // "td" `atDepth` 1) $ inSerial $ do
                           _ <- seekNext $ html "span" -- skip dropdown
                           _ <- stepNext $ html "span" -- skip dropdown
                           book <- T.strip <$> stepNext (text textSelector)
                           chapters <- stepNext $ text "span"
                           case T.decimal chapters of
                             Left _ -> return $ Left InternalError
                             Right (chapters', _) -> return $ Right (removeSpaces book, chapters'))

        removeSpaces = T.filter (not . isSpace)

scrapeBookChapter :: Text -> Text -> Text -> IO (Either Error Chapter)
scrapeBookChapter version book chapter = do
    ret <- scrapeURL (T.unpack link) chapterScraper
    case ret of
      Nothing -> return $ Left InternalError
      Just c  -> return c
  where link = "https://www.biblegateway.com/passage/?version=" <> version <> "&search=" <> book <> chapter

        chapterScraper :: Scraper Verse (Either Error Chapter)
        chapterScraper = do
          ret <- chroot ("div" @: [hasClass "passage-content"] // "div" `atDepth` 1) paragraphsScraper
          case ret of
            Left err     -> return $ Left err
            Right verses -> return $ Right verses''
              where verses' = sortBy (compare `on` fst) verses
                    verses'' = cleanVerses $ joinVerses verses'

        paragraphsScraper :: Scraper Verse (Either Error [(Int, Verse)])
        paragraphsScraper = sequence <$> chroots ("p" // "span" @: [ "class" @=~ verseSpanRE ]) verseScraper
          where verseSpanRE = re ("[[:alpha:]][[:alpha:]][[:alpha:]]-" <> chapter <> "-[[:digit:]]+")

        verseScraper :: Scraper Verse (Either Error (Int, Verse))
        verseScraper = do
          spanClass <- attr "class" "span"
          case reverse $ T.splitOn "-" spanClass of
            (index : _) ->
              case T.decimal index of
                Left _ -> return $ Left InvalidVerseSpan
                Right (index'', _) -> inSerial $ do
                  verse <- T.concat <$> many (stepNext innerSpanScraper)
                  return $ Right (index'', verse)

            _                 -> return $ Left InvalidVerseSpan

        innerSpanScraper :: Scraper Verse Verse
        innerSpanScraper = sup <|> chapterNum <|> bareText
          where sup = text "sup" >> return ""
                chapterNum = text ("span" @: [ hasClass "chapternum" ]) >> return ""
                bareText = text textSelector

----

downloadBible :: FilePath -> Text -> Text -> IO ()
downloadBible dir versionShort versionLong = do
  ret <- scrapeBibleBooks versionLong
  case ret of
    Left err    -> hPutStrLn stderr $ "Error '" ++ show err ++ "' while downloading bible booklist"
    Right books -> do
      bookThreads <- forM books (async . uncurry (downloadBook dir versionShort))
      mapM_ wait bookThreads

downloadBook :: FilePath -> Text -> Text -> Int -> IO ()
downloadBook dir version bookName bookSize = do
    T.putStrLn $ "Downloading " <> bookName <> "..."
    createDirectory directory
    forM_ [1..bookSize] (downloadBookChapter dir version bookName)
  where directory = dir ++ "/" ++ T.unpack bookName

downloadBookChapter :: FilePath -> Text -> Text -> Int -> IO ()
downloadBookChapter dir version bookName chapterNum = do
    chapter <- scrapeBookChapter version bookName chapterName
    case chapter of
      Left err       -> T.hPutStrLn stderr $ "Error '" <> T.pack (show err) <> "' while downloading '" <> bookName <> ":" <> T.pack (show chapterNum) <> "'"
      Right chapter' -> T.writeFile file $ T.unlines chapter'
  where chapterName = T.pack $ show chapterNum
        file = dir ++ "/" ++ T.unpack bookName ++ "/" ++ T.unpack chapterName

----

createDirectory :: FilePath -> IO ()
createDirectory = createDirectoryIfMissing True

re :: Text -> Text.Regex.TDFA.Regex
re = Text.Regex.TDFA.makeRegex

joinVerses :: [(Int, Verse)] -> [Verse]
joinVerses [] = []
joinVerses ((k, v):kvs) = T.intercalate " " (v:vs) : padding ++ joinVerses kvs'
  where (vs', kvs') = span (\(k', _) -> k == k') kvs
        vs = map snd vs'
        leap = case kvs' of
          []          -> 1
          ((k', _):_) -> k' - k
        padding = replicate (leap - 1) ""

cleanVerses :: [Verse] -> [Verse]
cleanVerses = map (T.replace "′" "'" . T.strip)

