{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative      (Alternative (many), (<|>))
import           Control.Concurrent.Async (async, wait)
import           Control.Monad            (forM, forM_)
import           Data.Maybe               (fromJust)
import           Data.Strings             (strTrim)
import           System.Directory         (createDirectoryIfMissing)
import           Text.HTML.Scalpel        (Scraper, atDepth, chroot, chroots,
                                           hasClass, html, inSerial, scrapeURL,
                                           seekNext, stepNext, text,
                                           textSelector, (//), (@:))

type Verse = String
type Chapter = [Verse]

longName :: String
longName = "Revised-Standard-Version-Catholic-Edition-RSVCE-Bible"

shortName :: String
shortName = "RSVCE"

main :: IO ()
main = downloadBible "./bible/rsvce" shortName longName

----

scrapeBibleBooks :: String -> IO (Maybe [(String, Int)])
scrapeBibleBooks version = scrapeURL link booksScraper
  where link = "https://www.biblegateway.com/versions/" ++ version ++ "/#booklist"

        booksScraper = chroot ("table" @: [hasClass "chapterlinks"]) $ inSerial $ many $
                         seekNext $ chroot ("tr" // "td" `atDepth` 1) $ inSerial $ do
                           _ <- seekNext $ html "span" -- skip dropdown
                           _ <- stepNext $ html "span" -- skip dropdown
                           book <- strTrim <$> stepNext (text textSelector)
                           chapters <- stepNext $ text "span"
                           return (book, read chapters)

scrapeBookChapter :: String -> String -> String -> IO (Maybe Chapter)
scrapeBookChapter version book chapter = scrapeURL link chapterScraper
  where link = "https://www.biblegateway.com/passage/?version=" ++ version ++ "&search=" ++ book ++ chapter

        chapterScraper :: Scraper String Chapter
        chapterScraper = chroot ("div" @: [hasClass "passage-content"] // "div" `atDepth` 1) paragraphsScraper

        paragraphsScraper :: Scraper String [Verse]
        paragraphsScraper = chroots ("p" // "span" `atDepth` 1) verseScraper

        verseScraper :: Scraper String Verse
        verseScraper = chroot "span" . inSerial $ do
          _ <- stepNext $ text textSelector -- skip over chapter/verse num
          concat <$> many (stepNext innerScraper)
          where innerScraper = tag <|> bareText
                tag = text "sup" >> return ""
                bareText = text textSelector

----

downloadBible :: String -> String -> String -> IO ()
downloadBible baseDir versionShort versionLong = do
  ret <- scrapeBibleBooks versionLong
  case ret of
    Nothing    -> return ()
    Just books -> do
      bookThreads <- forM books (async . uncurry (downloadBook baseDir versionShort))
      mapM_ wait bookThreads

downloadBook :: String -> String -> String -> Int -> IO ()
downloadBook baseDir version bookName bookSize = do
    putStrLn $ "Downloading " ++ bookName ++ "..."
    createDirectory directory
    forM_ [1..bookSize] (downloadBookChapter baseDir version bookName)
  where directory = baseDir ++ "/" ++ bookName

downloadBookChapter :: String -> String -> String -> Int -> IO ()
downloadBookChapter baseDir version bookName chapterNum = do
    chapter <- scrapeBookChapter version bookName chapterName
    writeFile file $ unlines (fromJust chapter)
  where chapterName = show chapterNum
        file = baseDir ++ "/" ++ bookName ++ "/" ++ chapterName

createDirectory :: FilePath -> IO ()
createDirectory = createDirectoryIfMissing True

