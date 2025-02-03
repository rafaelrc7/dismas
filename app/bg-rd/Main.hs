{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main where

import           Control.Monad                   (unless, when, (>=>))
import           Data.Bifunctor                  (first, second)
import           Data.Char                       (isSpace)
import           Data.Functor                    (void)
import           Data.List                       (sort, sortOn)
import           Data.Maybe                      (fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Text.AhoCorasick.Automaton (AcMachine, CodeUnitIndex,
                                                  Match (..))
import qualified Data.Text.AhoCorasick.Automaton as Aho
import qualified Data.Text.IO                    as T
import qualified Data.Text.Utf8                  as UTF
import           Prettyprinter                   (Doc, LayoutOptions (..),
                                                  PageWidth (..), annotate,
                                                  fill, indent, layoutPretty,
                                                  line, nest, pretty, vsep,
                                                  (<+>))
import           Prettyprinter.Render.Terminal   (AnsiStyle, bold, underlined)
import qualified Prettyprinter.Render.Terminal   as PPANSI
import qualified Prettyprinter.Render.Text       as PPT
import           Prettyprinter.Util              (reflow)
import           Settings                        (Book, Chapter, Query (..),
                                                  Reference (..), Search (..),
                                                  Settings (..), Verse,
                                                  parseCLIArgs)
import           System.Directory                (doesDirectoryExist,
                                                  listDirectory)
import           System.Environment.Blank        (getEnv)
import           System.Exit                     (exitFailure)
import           System.FilePath                 (dropTrailingPathSeparator,
                                                  takeBaseName, (</>))
import           System.IO                       (hClose, hIsTerminalDevice,
                                                  hPutStrLn, stderr, stdout)
import           System.IO.Error                 (isDoesNotExistError,
                                                  tryIOError)
import           System.Process                  (CreateProcess (..),
                                                  StdStream (..), proc,
                                                  waitForProcess,
                                                  withCreateProcess)

data Error = InvalidDir FilePath
           | InvalidBook Book
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

  let (references, searches) = partitionQueries (bibleQueries settings)
  let layoutOptions = LayoutOptions {layoutPageWidth = AvailablePerLine (fromIntegral (textWidth settings)) 1.0 }

  retSearches <- sequence <$> mapM (getSearch bibleDir) searches
  ret <- case retSearches of
    Left err -> return $ Left err
    Right searchTxts -> do
      let searchTxt = prettySBooks $ concat searchTxts

      retRefs <- sequence <$> mapM (getReferece bibleDir) references
      case retRefs of
        Left err -> return $ Left err
        Right refTxts -> do
          let refTxt = prettyBooks refTxts

          return $ Right $ searchTxt <> refTxt

  case ret of
    Left err -> printErr bibleDir err >> exitFailure
    Right txt -> do
      let txt' = layoutPretty layoutOptions txt

      isTerminal <- hIsTerminalDevice stdout
      if not isTerminal then
        PPT.renderIO stdout txt'
      else do
        pager <- getEnv "PAGER"
        case pager of
          Nothing     -> PPANSI.renderIO stdout txt'
          Just pager' -> withCreateProcess ((proc pager' []) { std_in = CreatePipe }) $ \stdin _ _ ph -> do
            case stdin of
              Just stdin' -> do
                PPANSI.renderIO stdin' txt'
                hClose stdin'
                void $ waitForProcess ph
              Nothing -> do
                hPutStrLn stderr $ "ERROR: Failed to write to '" <> pager' <> "' stdin"
                PPANSI.renderIO stdout txt'


printErr :: FilePath -> Error -> IO ()
printErr _ (InvalidDir dir) =
  T.hPutStrLn stderr $ "Bible does not exist at '" <> T.pack dir <> "'"
printErr bibleDir (InvalidBook book) =
  T.hPutStrLn stderr $ "Selected book '" <> book <> "' does not exist at '" <> T.pack bibleDir <> "'"
printErr bibleDir (InvalidBookChapter book chapter) =
  T.hPutStrLn stderr $ "Selected chapter '" <> book <> ":" <> T.pack (show chapter) <> "' does not exist at '" <> T.pack bibleDir <> "'"
printErr bibleDir (InvalidChapterVerse chapter verse) =
  T.hPutStrLn stderr $ "Selected verse '" <> T.pack (show chapter) <> ":" <> T.pack (show verse) <> "' does not exist at '" <> T.pack bibleDir <> "'"
printErr bibleDir (InvalidBookChapterVerse book chapter verse) =
  T.hPutStrLn stderr $ "Selected verse '" <> book <> ":" <> T.pack (show chapter) <> ":" <> T.pack (show verse) <> "' does not exist at '" <> T.pack bibleDir <> "'"

partitionQueries :: [Query] -> ([Reference], [Search])
partitionQueries = foldr partition ([], [])
  where partition :: Query -> ([Reference], [Search]) -> ([Reference], [Search])
        partition (Reference r) (rs, ss) = (r:rs, ss)
        partition (Search s) (rs, ss)    = (rs, s:ss)

----

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


getSearch :: FilePath -> Search -> IO (Either Error [(Book, [([(CodeUnitIndex, CodeUnitIndex)], ((Chapter, Verse), Text))])])
getSearch biblePath (SearchWhole needle) = do
  bookList <- fmap (map T.pack) <$> getBookList biblePath
  case bookList of
    Left err        -> return $ Left err
    Right bookList' -> do
      books <- sequence <$> mapM (getReferece biblePath . Book) bookList'
      case books of
        Left err     -> return $ Left err
        Right books' -> do
          let needle' = UTF.lowerUtf8 needle
          let searcher = Aho.build [(needle', needle')]
          let books'' = map (second (filterVerseSearch searcher)) books'
          return $ Right $ filter (not . null . snd) books''
getSearch biblePath (SearchBook book needle) = do
   bookTxt <- getReferece biblePath $ Book book
   case bookTxt of
     Left err     -> return $ Left err
     Right bookTxt' -> do
       let needle' = UTF.lowerUtf8 needle
       let searcher = Aho.build [(needle', needle')]
       let bookTxt'' = second (filterVerseSearch searcher) bookTxt'
       return $ Right $ filter (not . null . snd) [bookTxt'']
getSearch biblePath (SearchBookChapter book chapter needle) = do
   bookTxt <- getReferece biblePath $ BookChapter book chapter
   case bookTxt of
     Left err     -> return $ Left err
     Right bookTxt' -> do
       let needle' = UTF.lowerUtf8 needle
       let searcher = Aho.build [(needle', needle')]
       let bookTxt'' = second (filterVerseSearch searcher) bookTxt'
       return $ Right $ filter (not . null . snd) [bookTxt'']

----

getBookList :: FilePath -> IO (Either Error [FilePath])
getBookList biblePath = do
  verses <- tryIOError $ listDirectory biblePath
  case verses of
    Right bookList              -> return $ Right bookList
    Left err
      | isDoesNotExistError err -> return $ Left $ InvalidDir biblePath
      | otherwise               -> ioError err

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
    Right verses'               -> return $ Right $ zip [1..] $ T.lines verses'
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

filterVerseSearch :: AcMachine Text -> [((Chapter, Verse), Text)] -> [([(CodeUnitIndex, CodeUnitIndex)], ((Chapter, Verse), Text))]
filterVerseSearch searcher verses = verses''
  where allMatches = Aho.runWithCase Aho.IgnoreCase [] (\matches match -> Aho.Step (match : matches))
        searches = map (allMatches searcher . snd) verses
        verses' = filter (not . null . fst) $ zip searches verses
        getMatchIndices :: Match Text -> (CodeUnitIndex, CodeUnitIndex)
        getMatchIndices match = (matchPos match - len, len)
          where len = UTF.lengthUtf8 (matchValue match)
        verses'' = map (first (map getMatchIndices)) verses'

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

prettySVerses :: [([(CodeUnitIndex, CodeUnitIndex)], ((Chapter, Verse), Text))] -> Doc AnsiStyle
prettySVerses verses = vsep markedVerses
  where showVerseMark :: (Int, Int) -> Text
        showVerseMark (c, v) = T.pack (show c) <> ":" <> T.pack (show v)

        getMaxVerseMarkWidth :: [Text] -> Int
        getMaxVerseMarkWidth vs = maximum $ map T.length vs

        verses' = map (\(cuts, (m, t)) -> (m, annotateRanges bold cuts t)) verses

        (verseMarks, verseTexts) = unzip verses'
        verseMarks' = map showVerseMark verseMarks
        verseMarks'' = map (indent 2 . annotate bold . fill verseMarkWidth . pretty . showVerseMark) verseMarks
        verseMarkWidth = getMaxVerseMarkWidth verseMarks' + 1
        markedVerses = map (nest (verseMarkWidth + 3)) $ zipWith (<+>) verseMarks'' verseTexts

annotateRanges :: AnsiStyle -> [(CodeUnitIndex, CodeUnitIndex)] -> Text -> Doc AnsiStyle
annotateRanges style ranges = annotateRanges' style (sortOn fst ranges)

annotateRanges' :: AnsiStyle -> [(CodeUnitIndex, CodeUnitIndex)] -> Text -> Doc AnsiStyle
annotateRanges' _ [] t  = reflow t
annotateRanges' style ((begin, len):ms) t = t''
  where (tb, ta) = UTF.unsafeCutUtf8 begin len t
        ta' = annotateRanges' style ms' ta
        tb' = reflow tb
        s = UTF.unsafeSliceUtf8 begin len t
        s' = annotate style $ reflow s

        uncons = T.uncons
        unsnoc = T.unsnoc >=> (\(a, b) -> return (b, a))

        t' = if hasTrailingSpace uncons s || hasTrailingSpace unsnoc tb then
               tb' <+> s'
              else
               tb' <> s'
        t'' = if hasTrailingSpace uncons ta || hasTrailingSpace unsnoc s then
               t' <+> ta'
              else
               t' <> ta'
        ms' = map (\(b, l) -> (b-(begin+len), l)) ms

hasTrailingSpace :: (Text -> Maybe (Char, Text)) -> Text -> Bool
hasTrailingSpace get t = case get t of
  Nothing     -> False
  Just (c, _) -> isSpace c

prettySBook :: Text -> [([(CodeUnitIndex, CodeUnitIndex)], ((Chapter, Verse), Text))] -> Doc AnsiStyle
prettySBook book verses = book' <> line <> line <> verses'
  where book' = annotate (bold <> underlined) $ "Book of" <+> pretty book
        verses' = prettySVerses verses

prettySBooks :: [(Book, [([(CodeUnitIndex, CodeUnitIndex)], ((Chapter, Verse), Text))])] -> Doc AnsiStyle
prettySBooks books = txt
  where books' = map ((<> line) . uncurry prettySBook) books
        txt = vsep books' <> if not (null books') then line else mempty
