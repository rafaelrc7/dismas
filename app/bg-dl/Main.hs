{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative      (Alternative (many), (<|>))
import           Control.Concurrent.Async (async, wait)
import           Control.Monad            (forM, forM_, unless, void)
import           Data.Char                (isSpace)
import           Data.Maybe               (fromJust)
import           Data.Strings             (strTrim)
import           Settings                 (Settings (baseDir, noConfirm, versionLongName, versionShortName),
                                           parseCLIArgs)
import           System.Directory         (createDirectoryIfMissing)
import           Text.HTML.Scalpel        (Scraper, atDepth, chroot, chroots,
                                           hasClass, html, inSerial, scrapeURL,
                                           seekNext, stepNext, text,
                                           textSelector, (//), (@:))

type Verse = String
type Chapter = [Verse]

main :: IO ()
main = do
  settings <- parseCLIArgs
  let dir = baseDir settings ++ "/" ++ versionShortName settings
  putStrLn $ "Will download the '" ++ versionShortName settings ++ "' from the biblegateway site to '" ++ dir ++ "'"
  unless (noConfirm settings) $ do
    putStrLn "Press ENTER to continue or ^C to cancel..."
    void getLine
  downloadBible dir (versionShortName settings) (versionLongName settings)

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
                           return (removeSpaces book, read chapters)

        removeSpaces = filter (not . isSpace)

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
downloadBible dir versionShort versionLong = do
  ret <- scrapeBibleBooks versionLong
  case ret of
    Nothing    -> return ()
    Just books -> do
      bookThreads <- forM books (async . uncurry (downloadBook dir versionShort))
      mapM_ wait bookThreads

downloadBook :: String -> String -> String -> Int -> IO ()
downloadBook dir version bookName bookSize = do
    putStrLn $ "Downloading " ++ bookName ++ "..."
    createDirectory directory
    forM_ [1..bookSize] (downloadBookChapter dir version bookName)
  where directory = dir ++ "/" ++ bookName

downloadBookChapter :: String -> String -> String -> Int -> IO ()
downloadBookChapter dir version bookName chapterNum = do
    chapter <- scrapeBookChapter version bookName chapterName
    writeFile file $ unlines (fromJust chapter)
  where chapterName = show chapterNum
        file = dir ++ "/" ++ bookName ++ "/" ++ chapterName

----

createDirectory :: FilePath -> IO ()
createDirectory = createDirectoryIfMissing True

