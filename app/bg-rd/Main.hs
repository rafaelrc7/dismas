{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Monad                 (unless, when)
import           Data.Functor                  (void)
import           Data.List                     (sort)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Prettyprinter                 (Doc, LayoutOptions (..),
                                                PageWidth (..), annotate, fill,
                                                indent, layoutPretty, line,
                                                nest, pretty, vsep, (<+>))
import           Prettyprinter.Render.Terminal (AnsiStyle, bold, underlined)
import qualified Prettyprinter.Render.Terminal as PPANSI
import qualified Prettyprinter.Render.Text     as PPT
import           Prettyprinter.Util            (reflow)
import           Settings                      (Book, Chapter, Reference (..),
                                                Settings (..), Verse,
                                                parseCLIArgs)
import           System.Directory              (doesDirectoryExist,
                                                listDirectory)
import           System.Environment.Blank      (getEnv)
import           System.Exit                   (exitFailure)
import           System.FilePath               (dropTrailingPathSeparator,
                                                takeBaseName, (</>))
import           System.IO                     (hClose, hIsTerminalDevice,
                                                hPutStrLn, stderr, stdout)
import           System.IO.Error               (isDoesNotExistError, tryIOError)
import           System.Process                (CreateProcess (..),
                                                StdStream (..), proc,
                                                waitForProcess,
                                                withCreateProcess)

data Error = InvalidBook Book
           | InvalidBookChapter Book Chapter
           | InvalidChapterVerse Chapter Verse
           | InvalidBookChapterVerse Book Chapter Verse
  deriving (Show)

main :: IO ()
main = do
  settings <- parseCLIArgs
  let bibleDir = baseDir settings </> T.unpack (bibleVersion settings)

  doesDirectoryExist bibleDir >>= \dirExists -> unless dirExists $ do
    hPutStrLn stderr $ "Selected bible '" ++ bibleDir ++ "' does not exist"
    exitFailure

  ret <- mapM (getReferece bibleDir) (bibleReference settings)

  case sequence ret of
    Right txts -> do
      let layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine (fromIntegral (textWidth settings)) 1.0 }
      let txt = layoutPretty layoutOptions $ prettyBooks txts
      isTerminal <- hIsTerminalDevice stdout
      if not isTerminal then
        PPT.renderIO stdout txt
      else do
        pager <- getEnv "PAGER"
        case pager of
          Nothing     -> PPANSI.renderIO stdout txt
          Just pager' -> withCreateProcess ((proc pager' []) { std_in = CreatePipe }) $ \stdin _ _ ph -> do
            case stdin of
              Just stdin' -> do
                PPANSI.renderIO stdin' txt
                hClose stdin'
                void $ waitForProcess ph
              Nothing -> do
                hPutStrLn stderr $ "ERROR: Failed to write to '" <> pager' <> "' stdin"
                PPANSI.renderIO stdout txt
    Left err -> do
      case err of
        InvalidBook book ->
          T.hPutStrLn stderr $ "Selected book '" <> book <> "' does not exist at '" <> T.pack bibleDir <> "'"
        InvalidBookChapter book chapter ->
          T.hPutStrLn stderr $ "Selected chapter '" <> book <> ":" <> T.pack (show chapter) <> "' does not exist at '" <> T.pack bibleDir <> "'"
        InvalidChapterVerse chapter verse ->
          T.hPutStrLn stderr $ "Selected verse '" <> T.pack (show chapter) <> ":" <> T.pack (show verse) <> "' does not exist at '" <> T.pack bibleDir <> "'"
        InvalidBookChapterVerse book chapter verse ->
          T.hPutStrLn stderr $ "Selected verse '" <> book <> ":" <> T.pack (show chapter) <> ":" <> T.pack (show verse) <> "' does not exist at '" <> T.pack bibleDir <> "'"
      exitFailure

getReferece :: FilePath -> Reference -> IO (Either Error (Book, [((Chapter, Verse), Text)]))
getReferece biblePath (Book book) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getChapterList path
      text <- getBook path chapters
      case text of
        Left err    -> return $ Left err
        Right text' -> return $ Right (book, text')
getReferece biblePath (BookChapter book chapter) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      text <- getBook path [chapter]
      case text of
        Left err    -> return $ Left err
        Right text' -> return $ Right (book, text')
getReferece biblePath (BookChapterRange book chapter1 chapter2) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getChapterList path
      let selectedChapters = getChapterRange (length chapters) (Just chapter1) (Just chapter2)
      text <- getBook path selectedChapters
      case text of
        Left err    -> return $ Left err
        Right text' -> return $ Right (book, text')
getReferece biblePath (BookChapterVerses book chapter verses) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      let verses' = map (chapter, ) verses
      text <- getBook path [chapter]
      case text of
        Left err -> return $ Left err
        Right text' -> do
          let text'' = filterVerses verses' text'
          case text'' of
            Left (InvalidChapterVerse c v) -> return $ Left (InvalidBookChapterVerse book c v)
            Left err                       -> return $ Left err
            Right text'''                  -> return $ Right (book, text''')
getReferece biblePath (BookChapterVerseRange book chapter verse1 verse2) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      text <- getBook path [chapter]
      case text of
        Left err -> return $ Left err
        Right text' -> do
          let text'' = filterVerseRange (Just (chapter, verse1)) (Just (chapter, verse2)) text'
          case text'' of
            Left (InvalidChapterVerse c v) -> return $ Left (InvalidBookChapterVerse book c v)
            Left err                       -> return $ Left err
            Right text'''                  -> return $ Right (book, text''')
getReferece biblePath (BookChaptersVerseRange book chapter1 verse1 chapter2 verse2) = do
  path' <- getBookPath biblePath book
  case path' of
    Left err -> return $ Left err
    Right path -> do
      chapters <- getChapterList path
      let selectedChapters = getChapterRange (length chapters) (Just chapter1) (Just chapter2)
      text <- getBook path selectedChapters
      case text of
        Left err    -> return $ Left err
        Right text' -> do
          case filterVerseRange (Just (chapter1, verse1)) (Just (chapter2, verse2)) text' of
            Left (InvalidChapterVerse c v) -> return $ Left (InvalidBookChapterVerse book c v)
            Left err                       -> return $ Left err
            Right text''                   -> return $ Right (book, text'')

----

getBookList :: FilePath -> IO [FilePath]
getBookList = listDirectory

getBookPath :: FilePath -> Text -> IO (Either Error FilePath)
getBookPath biblePath book = do
  let path = biblePath </> T.unpack book
  exists <- doesDirectoryExist path
  if exists then
    return $ Right path
  else
    return $ Left (InvalidBook book)

----

getChapterList :: FilePath -> IO [Int]
getChapterList bookPath = sort . map read <$> listDirectory bookPath

getChapter :: FilePath -> Int -> IO (Either Error [(Int, Text)])
getChapter bookPath chapter = do
  let book = takeBaseName $ dropTrailingPathSeparator bookPath
  let chapterPath = bookPath </> show chapter
  verses <- tryIOError $ T.readFile chapterPath
  case verses of
    Right verses'                       -> return $ Right $ zip [1..] $ T.lines verses'
    Left err
      | isDoesNotExistError err -> return $ Left $ InvalidBookChapter (T.pack book) chapter
      | otherwise               -> ioError err

getChapters :: FilePath -> [Int] -> IO (Either Error [(Int, [(Int, Text)])])
getChapters bookPath chapters = fmap (zip chapters) . sequence <$> mapM (getChapter bookPath) chapters

getBook :: FilePath -> [Int] -> IO (Either Error [((Int, Int), Text)])
getBook  bookPath chapters = fmap joinChapterVerseNumber <$> getChapters bookPath chapters

joinChapterVerseNumber :: [(Int, [(Int, Text)])] -> [((Int, Int), Text)]
joinChapterVerseNumber = concatMap (uncurry addChapterNumber')
  where addChapterNumber' :: Int -> [(Int, Text)] -> [((Int, Int), Text)]
        addChapterNumber' c = map (\(v, t) -> ((c, v), t))

----

getChapterRange :: Int -> Maybe Int -> Maybe Int -> [Int]
getChapterRange chapterNum from to = [fromMaybe 1 from..fromMaybe chapterNum to]

filterVerses :: [(Int, Int)] -> [((Int, Int), Text)] -> Either Error [((Int, Int), Text)]
filterVerses []           _  = Right []
filterVerses ((sc, sv):_) [] = Left (InvalidChapterVerse sc sv)
filterVerses (sv:svs)     vs = do
  (selv, vs') <- filterVerse sv vs
  selvs       <- filterVerses svs vs'
  return (selv : selvs)

filterVerse :: (Int, Int) -> [((Int, Int), Text)] -> Either Error (((Int, Int), Text), [((Int, Int), Text)])
filterVerse (sc, sv) [] = Left (InvalidChapterVerse sc sv)
filterVerse s@(sc, sv) (v@(m@(mc, mv), _):vs) = do
  when (mc > sc || mc == sc && mv > sv) $ Left (InvalidChapterVerse sc sv)
  if s == m then
    Right (v, vs)
  else
    filterVerse s vs

filterVerseRange :: Maybe (Int, Int) -> Maybe (Int, Int) -> [((Int, Int), Text)] -> Either Error [((Int, Int), Text)]
filterVerseRange from to verses = do
  verses' <- case from of
    Just from' -> filterVersesFrom from' verses
    Nothing    -> Right verses

  case to of
    Just to' -> filterVersesTo to' verses'
    Nothing  -> Right verses'

filterVersesTo :: (Int, Int) -> [((Int, Int), Text)] -> Either Error [((Int, Int), Text)]
filterVersesTo (toc, tov) [] = Left (InvalidChapterVerse toc tov)
filterVersesTo to@(toc, tov) (v@(vr@(vrc, _), _):vs)
  | vr == to  = Right [v]
  | vrc > toc = Left (InvalidChapterVerse toc tov)
  | otherwise = (v :) <$> filterVersesTo to vs

filterVersesFrom :: (Int, Int) -> [((Int, Int), Text)] -> Either Error [((Int, Int), Text)]
filterVersesFrom (fromc, fromv)      [] = Left (InvalidChapterVerse fromc fromv)
filterVersesFrom from@(fromc, fromv) vs'@((vr@(vrc, _), _):vs)
  | vr == from  = Right vs'
  | vrc > fromc = Left (InvalidChapterVerse fromc fromv)
  | otherwise = filterVersesFrom from vs

----

prettyVerses :: [((Int, Int), Text)] -> Doc AnsiStyle
prettyVerses verses = vsep markedVerses
  where showVerseMark :: (Int, Int) -> Text
        showVerseMark (c, v) = T.pack (show c) <> ":" <> T.pack (show v)

        getMaxVerseMarkWidth :: [Text] -> Int
        getMaxVerseMarkWidth vs = maximum $ map T.length vs

        (verseMarks, verseTexts) = unzip verses
        verseMarks' = map showVerseMark verseMarks
        verseMarks'' = map (indent 2 . annotate bold . fill verseMarkWidth . pretty . showVerseMark) verseMarks
        verseMarkWidth = getMaxVerseMarkWidth verseMarks' + 1
        markedVerses = map (nest (verseMarkWidth + 3)) $ zipWith (<+>) verseMarks'' (map reflow verseTexts)

prettyBook :: Text -> [((Int, Int), Text)] -> Doc AnsiStyle
prettyBook book verses = book' <> line <> line <> verses'
  where book' = annotate (bold <> underlined) $ "Book of" <+> pretty book
        verses' = prettyVerses verses

prettyBooks :: [(Text, [((Int, Int), Text)])] -> Doc AnsiStyle
prettyBooks books = vsep $ map ((<> line) . uncurry prettyBook) books

