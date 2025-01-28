{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad    (unless)
import           Data.List        (sort)
import           Data.Text        (Text)
import qualified Data.Text        as T
import qualified Data.Text.IO     as T
import           Settings         (Reference (..), Settings (..), parseCLIArgs)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.Exit      (exitFailure)
import           System.IO        (hPutStrLn, stderr)

data Error = InvalidBook Text
           | InvalidChapter Text Text
           | InvalidVerse Text Text Text
  deriving (Show)

main :: IO ()
main = do
  settings <- parseCLIArgs
  let bibleDir = baseDir settings ++ "/" ++ T.unpack (bibleVersion settings)

  doesDirectoryExist bibleDir >>= \dirExists -> unless dirExists $ do
    hPutStrLn stderr $ "Selected bible '" ++ bibleDir ++ "' does not exist"
    exitFailure

  ret <- getReferece bibleDir (bibleReference settings)

  case ret of
    Right txt -> T.putStrLn txt
    Left err -> do
      case err of
        InvalidBook book ->
          T.hPutStrLn stderr $ "Selected book '" <> book <> "' does not exist at '" <> T.pack bibleDir <> "'"
        InvalidChapter book chapter ->
          T.hPutStrLn stderr $ "Selected chapter '" <> chapter <> "' does not exist in book '"
            <> book <> "' at '" <> T.pack bibleDir <> "'"
        InvalidVerse book chapter verse ->
          T.hPutStrLn stderr $ "Selected verse '" <> verse <> "' does not exist in book '"
            <> book <> ":" <> chapter <> "' at '" <> T.pack bibleDir <> "'"
      exitFailure

getReferece :: FilePath -> Reference -> IO (Either Error Text)
getReferece biblePath (Book book) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getBookChapters path
      let chaptersPaths = map (\chapter -> path ++ "/" ++ chapter) chapters
      chaptersTexts <- mapM T.readFile chaptersPaths
      let chaptersTexts' = addChapterNumbers $ map addVerseNumbers chaptersTexts
      return $ Right $ T.unlines chaptersTexts'
getReferece biblePath (BookChapter book chapter) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getBookChapters path
      let chapter' = show chapter
      if chapter' `notElem` chapters then
        return $ Left $ InvalidChapter book (T.pack chapter')
      else do
        let chapterPath = path ++ "/" ++ chapter'
        chapterText <- T.readFile chapterPath
        let chapterText' = addChapterNumber chapter $ addVerseNumbers chapterText
        return $ Right chapterText'
getReferece biblePath (BookChapterRange book chapter1 chapter2) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getBookChapters path
      let chapter1' = show chapter1
      let chapter2' = show chapter2
      if chapter1' `notElem` chapters then
        return $ Left $ InvalidChapter book (T.pack chapter1')
      else if chapter2' `notElem` chapters then
        return $ Left $ InvalidChapter book (T.pack chapter2')
      else do
        let selectedChapters = [chapter1..chapter2]
        let chaptersPaths = map (\chapter -> path ++ "/" ++ show chapter) selectedChapters
        chaptersTexts <- mapM T.readFile chaptersPaths
        let chaptersTexts' = addChapterNumbersFrom chapter1 $ map addVerseNumbers chaptersTexts
        return $ Right $ T.unlines chaptersTexts'
getReferece biblePath (BookChapterVerses book chapter verses) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getBookChapters path
      let chapter' = show chapter
      if chapter' `notElem` chapters then
        return $ Left $ InvalidChapter book (T.pack chapter')
      else do
        let chapterPath = path ++ "/" ++ chapter'
        chapterText <- T.readFile chapterPath
        let chapterVerses = T.lines chapterText
        let chapterSize = length chapterVerses
        let verses' = mapM (\verse ->
                          if verse < 1 || verse > chapterSize then
                            Left (InvalidVerse book (T.pack $ show chapter) (T.pack $ show verse))
                          else
                            Right (verse, chapterVerses !! (verse-1)))
                        verses
        case verses' of
          Left err         -> return $ Left err
          Right versesText -> return $ Right $ addChapterNumber chapter $ T.unlines $ addVersesNumbers versesText
getReferece biblePath (BookChapterVerseRange book chapter verse1 verse2) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getBookChapters path
      let chapter' = show chapter
      if chapter' `notElem` chapters then
        return $ Left $ InvalidChapter book (T.pack chapter')
      else do
        let chapterPath = path ++ "/" ++ chapter'
        chapterText <- T.readFile chapterPath
        let chapterVerses = T.lines chapterText
        let chapterSize = length chapterVerses
        if verse1 < 1 || verse1 > chapterSize then
          return $ Left (InvalidVerse book (T.pack $ show chapter) (T.pack $ show verse1))
        else if verse2 < 1 || verse2 > chapterSize then
          return $ Left (InvalidVerse book (T.pack $ show chapter) (T.pack $ show verse2))
        else do
          let versesText = T.unlines $ take (verse2-verse1+1) $ drop (verse1-1) chapterVerses
          let versesText' = addChapterNumber chapter $ addVerseNumbersFrom verse1 versesText
          return $ Right versesText'
getReferece biblePath (BookChaptersVerseRange book chapter1 verse1 chapter2 verse2) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getBookChapters path
      let chapter1' = show chapter1
      let chapter2' = show chapter2
      if chapter1' `notElem` chapters then
        return $ Left $ InvalidChapter book (T.pack chapter1')
      else if chapter2' `notElem` chapters then
        return $ Left $ InvalidChapter book (T.pack chapter2')
      else do
        let selectedChapters = [chapter1..chapter2]
        let chaptersPaths = map (\chapter -> path ++ "/" ++ show chapter) selectedChapters
        chaptersTexts <- mapM T.readFile chaptersPaths
        let chaptersTextsSize = length chaptersTexts
        if null chaptersTexts then
          return $ Left $ InvalidChapter book (T.pack chapter1')
        else do
          let chapter1Verses = T.lines $ head chaptersTexts
          let chapter2Verses = T.lines $ last chaptersTexts
          let chapter1Size = length chapter1Verses
          let chapter2Size = length chapter2Verses
          if verse1 < 1 || verse1 > chapter1Size then
            return $ Left (InvalidVerse book (T.pack $ show chapter1) (T.pack $ show verse1))
          else if verse2 < 1 || verse2 > chapter2Size then
            return $ Left (InvalidVerse book (T.pack $ show chapter2) (T.pack $ show verse2))
          else do
            if chapter1 /= chapter2 then do
              let chapter1Verses' = addVerseNumbersFrom verse1 $ T.unlines $ drop (verse1-1) chapter1Verses
              let chapter2Verses' = addVerseNumbers $ T.unlines $ take verse2     chapter2Verses
              let remainingChapters = map addVerseNumbers $ take (chaptersTextsSize-2) $ drop 1 chaptersTexts
              let selectedVerses = chapter1Verses' : remainingChapters ++ [chapter2Verses']
              let selectedVerses' = addChapterNumbersFrom chapter1 selectedVerses
              return $ Right $ T.unlines selectedVerses'
            else do
              let versesText = T.unlines $ take (verse2-verse1+1) $ drop (verse1-1) chapter1Verses
              let versesText' = addChapterNumber chapter1 $ addVerseNumbersFrom verse1 versesText
              return $ Right versesText'

getBookPath :: FilePath -> Text -> IO (Either Error FilePath)
getBookPath biblePath book = do
  let path = biblePath ++ "/" ++ T.unpack book
  exists <- doesDirectoryExist path
  if exists then
    return $ Right path
  else
    return $ Left $ InvalidBook book

getBookChapters :: FilePath -> IO [FilePath]
getBookChapters bookPath = do
  verses <- listDirectory bookPath
  return $ map show $ sort $ map (read :: FilePath -> Int) verses

addVerseNumbers :: Text -> Text
addVerseNumbers = addVerseNumbersFrom 1

addChapterNumbers :: [Text] -> [Text]
addChapterNumbers = addChapterNumbersFrom 1

addVerseNumbersFrom :: Int -> Text -> Text
addVerseNumbersFrom start chapterText = T.unlines . addVersesNumbers $ verses'
  where verses = T.lines chapterText
        verses' = zip [start..] verses

addVersesNumbers :: [(Int, Text)] -> [Text]
addVersesNumbers = map (\(n, t) -> T.pack (show n) <> ". " <> t)

addChapterNumbersFrom :: Int -> [Text] -> [Text]
addChapterNumbersFrom start chapters = chapters''
  where chapters' = zip [start..] chapters
        chapters'' = map (uncurry addChapterNumber) chapters'

addChapterNumber :: Int -> Text -> Text
addChapterNumber chapter chapterText = "Chapter " <> T.pack (show chapter) <> "\n" <> chapterText

