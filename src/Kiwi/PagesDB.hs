{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Kiwi.PagesDB
  ( updatePagesDB
  , reloadPages
  , pagesCount
  , warmUpSearchEngine
  , emptyPagesDB
  , brokenLinks
  , setKeywords      -- exported for tests
  ) where

import           Control.Concurrent (forkIO, getNumCapabilities)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception (IOException, evaluate, try)
import           Control.Monad (filterM, forM, forM_, unless, void, when)
import qualified Crypto.Random as CR
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import           Data.IORef (atomicWriteIORef, readIORef)
import           Data.List (find, partition, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isNothing)
import           Data.SearchEngine (insertDocs, deleteDoc)
import qualified Data.SearchEngine as SE
import qualified Data.Set as S
import qualified Data.Text as T
-- import qualified Data.Time.Calendar as Cal
-- import qualified Data.Time.Clock as C
import qualified System.Directory as D
import qualified System.FilePath as FP
import           Text.Printf (printf)

import           Kiwi.Edit (writePageFile)
import qualified Kiwi.Files as Files
import qualified Kiwi.Git as Git
import           Kiwi.Keywords (emptyKeywords, updateKeywords)
import           Kiwi.Related (withKeywords)
import qualified Kiwi.Log as L
import           Kiwi.Meta (isValidUid, uidFromBytes, writePage)
import           Kiwi.Search
import           Kiwi.Types
import           Kiwi.Pandoc (Loaded (..), loadPageIO, pagePath)
import           Kiwi.Tags (tagSubSegments, segmentsToTag)
import           Kiwi.Utils (traverseDir, nodup, isDotFile, isValidMdFilename, timeIO)
import qualified Utils.DocIndex as DI
import qualified Utils.DocIndex.Restricted as RDI

-- | Reloads the pages of the kiwi (see 'updatePagesDB'). The caller holds
-- 'pagesDBLock', so that two reloads (or a reload and a page save) can not
-- overwrite each other; readers are never blocked.
reloadPages :: ServerState -> IO ()
reloadPages ss = do
  db' <- readIORef (pagesDB ss)
  (db, t) <- timeIO $ updatePagesDB (logger ss) (contentDir ss) db'
  atomicWriteIORef (pagesDB ss) db
  warmUpSearchEngine (searchEngine db)
  logger ss L.Info $ T.pack $ concat [ "pages reloaded in ", printf "%.2f" t
                                     , "s, ", pagesCount db, " pages" ]

pagesCount :: PagesDB -> String
pagesCount = show . RDI.cardinal . pagesIndex

-- | Evaluate the search engine in a thread, so that the first search after
-- a load does not wait for it
warmUpSearchEngine :: PageSearchEngine -> IO ()
warmUpSearchEngine se = void $ forkIO $ void $ evaluate $ SE.invariant se


-- | Loads the files of the pages that are new or modified since they were
-- loaded, and removes the pages of the files that are gone, or reloaded. A
-- file is reloaded when its modification time differs from the one of its
-- loaded page, so that renamed files, and files with an old date (copied,
-- synchronized), are loaded. The
-- index and the search engine get the same removals and insertions.
--
-- Each source gets its mode (see 'SourceMode'): read-only when its config
-- says so, or when its folder is not the top of a git repository (logged).
-- Kiwi writes no file of a read-only source.
--
-- Each page gets a uid unique in its source (see 'resolveUids'): in a source
-- where kiwi writes, the files whose uid changes are written and committed
-- (see 'writePageFiles'), and a page whose file can not be written is not
-- loaded; in a read-only source, a page without a valid uid, or whose uid
-- is used, is left out.
--
-- The other files of the sources get a uid (see 'resolveFiles'), and are
-- listed in 'sourceFiles'.
updatePagesDB :: L.Logger -> FP.FilePath -> PagesDB -> IO PagesDB
updatePagesDB logger' contentDir' db = do
    sources <- filter (not . isDotFile) <$> D.listDirectory contentDir'
    -- the pages are anywhere in the folder of their source, except in the
    -- hidden folders (".git"…) and those starting with '_'
    sourceDirs <- filterM D.doesDirectoryExist $ map (FP.combine contentDir') sources
    modes <- fmap M.fromList $ forM sourceDirs $ \dir -> do
      let source = T.pack (FP.takeFileName dir)
          configured = any (\c -> scSourceName c == source && scReadOnly c) (sourcesConfig db)
      repo <- if configured then return False else Git.isRepositoryRoot dir
      unless (configured || repo) $ logger' L.Warn $ T.concat
        [ "source ", source, " : its folder is not the top of a git repository, read-only" ]
      return (source, if repo then WriteAllowed else ReadOnly)
    let writable source = M.lookup source modes == Just WriteAllowed
    fsPathsRaw <- concat <$> mapM (\d -> traverseDir d readName) sourceDirs
    -- the conflict copies of syncthing are not pages: they would take the
    -- uid of their page, or get a new one
    let conflictCopy = Files.isConflictCopy . T.pack . FP.takeFileName
        isPage p = isValidMdFilename (FP.takeFileName p) && not (conflictCopy p)
        (fsPaths, ignoredPaths) = partition isPage fsPathsRaw
    -- the other files (images…) are not pages
    let isPageExt p = FP.takeExtension p `elem` [".md", ".markdown"]
    forM_ (filter isPageExt ignoredPaths) $ \p -> logger' L.Debug $ T.pack $ concat $
      if conflictCopy p then [ "file ignored, ", p, " : conflict copy of syncthing" ]
      else [ "file ignored, ", p, " : a page file name starts with a letter or a digit"
           , ", and ends with .md or .markdown" ]
    let (servable, excluded) =
          partition (Files.servablePath . map T.pack . FP.splitDirectories . snd . sourcePath)
                    (filter (not . isPageExt) ignoredPaths)
    forM_ excluded $ \p -> logger' L.Debug $ T.pack $ concat
      [ "file ignored, ", p, " : temporary or backup file (see the README)" ]
    (files, fileErrors) <- resolveFiles logger' writable contentDir' (sourceFiles db)
                                                 (map sourcePath servable)
    fsPathsAndMTime <- mapM addMTime fsPaths
    let toReload = [ p | (p, mt) <- fsPathsAndMTime, isModified p mt ]
        keptPaths = S.difference (S.fromList fsPaths) (S.fromList toReload)
        -- the pages of the files that are gone, or reloaded: a reloaded file
        -- may give another page id, or no page if it can not be loaded
        removedPages = [ pg | pg <- dbPages, not $ S.member (pageAbsoluteFSPath pg) keptPaths ]
        removedUIDs = map pageUID removedPages
        pagesIndexClean = DI.removeMany (RDI.visibleIndex $ pagesIndex db) removedUIDs
        searchEngineClean = foldr deleteDoc (searchEngine db) removedUIDs
    (loaded, failed) <- loadPages toReload
    -- the pages of the index keep their uid
    resolved <- resolveUids logger' writable contentDir'
                  (S.fromList $ map pageUID $ DI.documentsList pagesIndexClean)
                  (sortOn (pageAbsoluteFSPath . loadedPage) loaded)
    let cmc = customMetaConfig db
        texts = [ (pageAbsoluteFSPath pg, writePage cmc h (pageBody pg))
                | (pg, Just h) <- resolved ]
    written <- if null texts then return M.empty
               else M.fromList <$> writePageFiles logger' contentDir' "uid assigned to" texts
    -- the time of a written file is read again, so that it is not reloaded
    inserted <- fmap concat $ forM resolved $ \(pg, _) ->
      case M.lookup (pageAbsoluteFSPath pg) written of
        Nothing -> return [pg]
        Just (Left _) -> return []
        Just (Right ()) -> do
          mtE <- try $ D.getModificationTime (pageAbsoluteFSPath pg)
          return [either (\(_ :: IOException) -> pg) (\mt -> pg { pageMTime = mt }) mtE]
    let indexRead = DI.insertMany pagesIndexClean inserted
        -- a file that is not loaded has no page in the index: it is loaded
        -- again at each reload, so these are all the errors
        errors = [ loadError p (T.pack e) | (p, e) <- failed ]
                 ++ [ loadError p (T.pack $ "unable to write the uid of the page : " ++ e)
                    | (p, Left e) <- M.toList written ]
    -- the links to missing pages are listed on the /doctor page, not logged:
    -- they are an issue of the content, not of the server
        newEngine = insertDocs inserted searchEngineClean
    -- the keywords of the pages read again, or of every page from time to
    -- time, as they depend on the other pages; given to these pages in the
    -- index, with the norm of their features for the related pages (see
    -- "Kiwi.Related")
    ((keywords, newIndex, full), t) <- timeIO $ do
      let (kw, computed, full') = updateKeywords (DI.documentsList indexRead) removedPages
                                                 inserted (dbKeywordsState db)
          idx = setKeywords cmc indexRead computed
      _ <- evaluate kw
      _ <- evaluate idx
      return (kw, idx, full')
    when full $ logger' L.Info $ T.pack $ "TF-IDF finished in " ++ printf "%.2f" t ++ "s"
    return db { dbGeneration = dbGeneration db + 1
              , dbSources = modes
              , loadErrors = sortOn (\(src, p, _) -> (src, p)) (errors ++ fileErrors)
              , sourceFiles = files
              , pagesIndex = RDI.whole newIndex
              , searchEngine = newEngine
              , dbKeywordsState = keywords }
  where
    -- TODO : gérer des conditions de rechargement
    -- getCondition = return False

    dbPages = RDI.documentsList $ pagesIndex db
    loadedByPath = M.fromList [ (pageAbsoluteFSPath pg, pg) | pg <- dbPages ]

    -- unknown file, or modification time different from the one of its page
    -- (a removed file has no time, and is then reloaded: loadPage reports it)
    isModified path mt = case M.lookup path loadedByPath of
      Nothing -> True
      Just pg -> mt /= Just (pageMTime pg)

    -- pages that can not be loaded are ignored, with a warning. The files
    -- are read and parsed concurrently, one thread by capability, each one
    -- with a slice of the paths: the results, and so the logs, are in the
    -- order of the paths.
    loadPages paths = do
      n <- getNumCapabilities
      let size = max 1 $ (length paths + n - 1) `div` n
          slices [] = []
          slices ps = let (s, rest) = splitAt size ps in s : slices rest
      results <- concat <$> mapConcurrently
                   (mapM (\p -> (p,) <$> loadPage contentDir' p)) (slices paths)
      forM_ [ (p, e) | (p, Left e) <- results ] $ \(p, e) ->
        logger' L.Warn $ T.pack $ concat ["page ignored, ", p, " : ", e]
      -- the lang may have been detected from the content
      forM_ [ (p, l) | (p, Right l) <- results ] $ \(p, l) -> do
        let pg = loadedPage l
        logger' L.Debug $ T.pack $ concat ["page loaded, ", p, " : ", show (pageLang pg)]
        -- typos, or custom metadata not declared in kiwi.ini
        unless (null $ loadedUnknownKeys l) $ logger' L.Warn $ T.concat
          [ "unknown metadata, ", T.pack p, " : ", T.intercalate ", " (loadedUnknownKeys l) ]
        forM_ (loadedInvalidValues l) $ \(k, v) ->
          logger' L.Warn $ T.concat [ "invalid metadata value, ", T.pack p, " : "
                                    , k, ": ", v ]
      return ( [ l | (_, Right l) <- results ]
             , [ (p, e) | (p, Left e) <- results ] )

    -- source of a file, and its path in the folder of the source
    sourcePath path =
      let source = fst $ pagePath contentDir' path
      in (source, FP.makeRelative (FP.joinPath [contentDir', T.unpack source]) path)
    -- the files and folders that are not read: hidden, or starting with '_'
    readName name = not (isDotFile name) && take 1 name /= "_"
    loadError path msg = let (source, rel) = sourcePath path in (source, rel, msg)

    -- file may have been removed since directory listing : no mtime, it
    -- will then be reported by loadPage
    addMTime path = do
      mtE <- try $ D.getModificationTime path
      return (path, either (\(_ :: IOException) -> Nothing) Just mtE)

    loadPage :: FP.FilePath -> FP.FilePath -> IO (Either String Loaded)
    loadPage cd fullPath =
      let source = fst $ pagePath cd fullPath
          md = defaultMeta db
          cmc = customMetaConfig db
          sc = find (\c -> scSourceName c == source) $ sourcesConfig db
      in loadPageIO fullPath source md cmc sc


-- | The loaded pages (in this order), each one with a uid unique in its
-- source, given the uids already used: a page keeps the uid of its file when
-- it is valid and not used, else it gets a new one, with a header to write
-- (that of the file, with this uid), when kiwi writes in its source (see
-- 'SourceMode'); in a read-only source, it is left out. The uid replaced,
-- if any, goes to the previous-uids.
resolveUids :: L.Logger -> (T.Text -> Bool) -> FP.FilePath -> S.Set PageUID -> [Loaded] ->
               IO [(Page, Maybe MetaData)]
resolveUids _ _ _ _ [] = return []
resolveUids logger' writable contentDir' used (l : ls) = do
  let pg = loadedPage l
      path = pageAbsoluteFSPath pg
      header = loadedHeader l
      source = fst $ pagePath contentDir' path
  case metaUid header of
    Just u | isValidUid u, not (S.member (source, u) used) -> next u Nothing
    fileUid | not (writable source) -> do
      logger' L.Debug $ T.concat
        [ "page ignored, ", T.pack path, " : "
        , maybe "no uid" (\u -> "uid " <> u <> if isValidUid u then " already used" else " invalid") fileUid
        , " in a read-only source" ]
      resolveUids logger' writable contentDir' used ls
    fileUid -> do
      u <- newUid source
      former <- case fileUid of
        Just old -> do
          logger' L.Warn $ T.concat
            [ "uid ", old, if isValidUid old then " already used" else " invalid"
            , ", ", T.pack path, " : new uid ", u ]
          return [old]
        Nothing -> do
          logger' L.Debug $ T.concat [ "uid assigned, ", T.pack path, " : ", u ]
          return []
      let previous = fromMaybe [] (metaPreviousUids header)
          previous' = previous ++ filter (`notElem` previous) former
          -- no empty previous-uids
          previousM = if null previous' then metaPreviousUids header else Just previous'
      next u (Just header { metaUid = Just u, metaPreviousUids = previousM })
  where
    next uid newHeader = do
      let source = fst $ pagePath contentDir' (pageAbsoluteFSPath $ loadedPage l)
          pg = loadedPage l
          pg' = pg { pageUID = (source, uid)
                   , pagePreviousUids = maybe (pagePreviousUids pg)
                                              (fromMaybe [] . metaPreviousUids) newHeader }
      ((pg', newHeader) :) <$> resolveUids logger' writable contentDir' (S.insert (source, uid) used) ls
    newUid source = do
      u <- uidFromBytes <$> (CR.getRandomBytes 10 :: IO BS.ByteString)
      if S.member (source, u) used then newUid source else return u

-- | Writes these texts of page files (see 'writePageFile'), of sources
-- where kiwi writes, and the result for each file. The files of each source
-- that differ from their last commit are first committed as they are (when
-- this commit fails, none of them is written), and the files written are
-- then committed, with the message "Kiwi: WHAT N pages". The failures are
-- logged, as errors for the writes (an issue of the system).
writePageFiles :: L.Logger -> FP.FilePath -> T.Text -> [(FP.FilePath, T.Text)] ->
                  IO [(FP.FilePath, Either String ())]
writePageFiles logger' contentDir' what texts = fmap concat $
  forM (M.toList $ M.fromListWith (flip (++)) [ (sourceOf p, [x]) | x@(p, _) <- texts ]) $
    \(source, ts) -> do
      let root = contentDir' FP.</> T.unpack source
      pre <- commitPaths logger' root "Kiwi: pre-update commit of" (count "pages" ts) (map fst ts)
      results <- case pre of
        Left e -> return [ (p, Left $ "not committed before its write: " ++ e) | (p, _) <- ts ]
        Right () -> forM ts $ \(p, t) ->
          (p,) . first (\(e :: IOException) -> show e) <$> try (writePageFile p t)
      forM_ [ (p, e) | (p, Left e) <- results ] $ \(p, e) ->
        logger' L.Error $ T.pack $ concat [ "unable to write the file ", p, " : ", e ]
      let ok = [ p | (p, Right ()) <- results ]
      unless (null ok) $ do
        logger' L.Info $ T.concat [ what, " ", T.pack (show $ length ok), " pages" ]
        void $ commitPaths logger' root (T.concat ["Kiwi: ", what]) (count "pages" ok) ok
      return results
  where sourceOf = fst . pagePath contentDir'

-- | "N WHAT", N being the number of items
count :: T.Text -> [a] -> T.Text
count what xs = T.concat [T.pack (show $ length xs), " ", what]

-- | Commits these files of the source whose folder is given, with the
-- message "PREFIX COUNT" (see 'Git.commitFiles'); the commit and the
-- failure are logged
commitPaths :: L.Logger -> FP.FilePath -> T.Text -> T.Text -> [FP.FilePath] -> IO (Either String ())
commitPaths logger' root prefix n paths = Git.commitFiles root paths "Kiwi" (T.concat [prefix, " ", n]) >>= \case
  Left e -> do
    logger' L.Warn $ T.pack $ concat [ "files not committed: ", e ]
    return $ Left e
  Right committed -> do
    unless (null committed) $ logger' L.Info $ T.concat
      [ prefix, ": ", T.pack (show $ length committed), " files committed" ]
    return $ Right ()


-- | The files of the sources that are not pages, given as (source, path
-- from the top of the source), each one with a uid unique in its source
-- (see "Kiwi.Files"): a file keeps the uid of its name when it is not used,
-- else it gets a new one, and is renamed. The files of the given table (the
-- previous load) that keep their path are considered first, then the others
-- in the order of their paths: a copy of a file gets a new uid, not the
-- file. In a source where kiwi writes, the files renamed are committed,
-- first as they are (when this commit fails, none of them is renamed), then
-- renamed, as for the pages (see 'writePageFiles'); in a read-only source,
-- the files that would be renamed are left out. Returns the table of the
-- files, and the files that could not be renamed (they have no uid), as
-- load errors.
resolveFiles :: L.Logger -> (T.Text -> Bool) -> FP.FilePath -> M.Map (T.Text, T.Text) FP.FilePath ->
                [(T.Text, FP.FilePath)] ->
                IO (M.Map (T.Text, T.Text) FP.FilePath, [(T.Text, FP.FilePath, T.Text)])
resolveFiles logger' writable contentDir' previous files = do
  assigned <- assign S.empty $ known ++ others
  let (renames, leftOut) = partition (\((source, _), _, _) -> writable source)
                             [ x | x@(_, path, new) <- assigned, path /= new ]
      full (source, _) path = FP.joinPath [contentDir', T.unpack source, path]
  forM_ leftOut $ \(k, path, _) -> logger' L.Debug $ T.pack $ concat
    [ "file ignored, ", full k path, " : no uid, or a uid used, in a read-only source" ]
  results <- fmap concat $
    forM (M.toList $ M.fromListWith (flip (++)) [ (fst k, [x]) | x@(k, _, _) <- renames ]) $
      \(source, rs) -> do
        let root = contentDir' FP.</> T.unpack source
        pre <- commitPaths logger' root "Kiwi: pre-update commit of" (count "files" rs)
                           [ full k p | (k, p, _) <- rs ]
        results <- forM rs $ \(k, path, new) -> do
          let target = full k new
          exists <- D.doesPathExist target
          r <- case pre of
            Left e -> return $ Left $ "not committed before its rename: " ++ e
            Right () | exists -> return $ Left "the new name is already used"
                     | otherwise -> first (\(e :: IOException) -> show e) <$>
                                      try (D.renamePath (full k path) target)
          case r of
            Left e -> logger' L.Error $ T.pack $ concat
                        [ "unable to rename the file ", full k path, " : ", e ]
            Right () -> logger' L.Debug $ T.concat [ "uid assigned, ", T.pack (full k new) ]
          return (k, path, new, r)
        let ok = [ (k, path, new) | (k, path, new, Right ()) <- results ]
        logger' L.Info $ T.concat [ "uid assigned to ", T.pack (show $ length ok), " files" ]
        unless (null ok) $ void $
          commitPaths logger' root "Kiwi: uid assigned to" (count "files" ok)
                      (concat [ [full k path, full k new] | (k, path, new) <- ok ])
        return results
  let failed = S.fromList $ [ k | (k, _, _, Left _) <- results ] ++ [ k | (k, _, _) <- leftOut ]
  return ( M.fromList [ (k, new) | (k, _, new) <- assigned, not (S.member k failed) ]
         , [ (fst k, path, T.pack $ "unable to rename the file to give it a uid : " ++ e)
           | (k, path, _, Left e) <- results ] )
  where
    nameUid path = T.pack <$> Files.fileUid (FP.takeFileName path)
    (known, others) = partition (\(source, path) -> case nameUid path of
                                   Just u -> M.lookup (source, u) previous == Just path
                                   Nothing -> False)
                                (sortOn id files)
    assign _ [] = return []
    assign used ((source, path) : fs) = case nameUid path of
      Just u | not (S.member (source, u) used) ->
        (((source, u), path, path) :) <$> assign (S.insert (source, u) used) fs
      _ -> do
        u <- newUid used source
        let new = FP.replaceFileName path $ Files.withFileUid (T.unpack u) (FP.takeFileName path)
        (((source, u), path, new) :) <$> assign (S.insert (source, u) used) fs
    newUid used source = do
      u <- uidFromBytes <$> (CR.getRandomBytes 10 :: IO BS.ByteString)
      if S.member (source, u) used then newUid used source else return u




-- | The links to pages that are not in the index, with the page of each one,
-- by file path of the page then target. Given the index restricted to the
-- pages a user can read, the links that are broken for this user.
brokenLinks :: RDI.RestrictedDocIndex Page PageUID MetaField DocIndexKey -> [(Page, PageUID)]
brokenLinks idx =
  [ (pg, uid) | pg <- sortOn pageAbsoluteFSPath $ RDI.documentsList idx
              , uid <- nodup $ pageLinks pg
              , isNothing $ RDI.findDoc idx uid ]


emptyPagesDB :: MetaData -> [CustomMetaConfig] -> [SourceConfig] -> PagesDB
emptyPagesDB md cmc sc =
    -- let oldDate = C.UTCTime (Cal.fromGregorian 2000 1 1) (C.secondsToDiffTime 0)
    PagesDB { defaultMeta = md
            , customMetaConfig = cmc
            , sourcesConfig = sc
            , dbSources = M.empty
            , dbGeneration = 0
            , loadErrors = []
            , sourceFiles = M.empty
            -- , lastUpdate = oldDate
            , pagesIndex = RDI.whole $ DI.empty pageUID (pageExtractKeys cmc)
            , searchEngine = initPageSearchEngine
            , dbKeywordsState = emptyKeywords }


pageExtractKeys :: [CustomMetaConfig] -> [DI.Field Page MetaField DocIndexKey]
pageExtractKeys cmc = [ mkField FieldSource KeyText ((:[]) . fst . pageUID)
                      , mkField FieldTag KeyText extractTags
                      , mkField FieldAccess KeyText (map fst . pageAccess)
                      , mkField FieldLink KeyPairText pageLinks
                      , mkField FieldFile KeyPairText (\p -> map (fst (pageUID p),) (pageFiles p))
                      , mkField FieldLang KeyLang ((:[]) . pageLang)
                      , mkField FieldArchive KeyBool ((:[]) . pageArchive)
                      , mkField FieldKeyword KeyText (map keywordStem . pageKeywords)
                      ] ++
                      map mkCustomField cmc
  where
    mkCustomField c =
      let fld = (FieldCustom (cmcName c))
          keys = \p -> fromMaybe [] $ findCmv (cmcName c) p
      in DI.Field fld keys
    findCmv n p = cmdKeys <$> find (\cmd -> cmdName cmd == n) (pageCustomMeta p)

-- | The pages of the index with these keywords (by page), and the norm of
-- their features for the related pages (see 'withKeywords'): only the
-- keywords are indexed again
setKeywords :: [CustomMetaConfig] -> DI.DocIndex Page PageUID MetaField DocIndexKey
            -> M.Map PageUID [Keyword] -> DI.DocIndex Page PageUID MetaField DocIndexKey
setKeywords cmc idx0 = M.foldlWithKey' set idx0
  where
    set idx uid ks = case DI.findDoc idx uid of
      Just p -> DI.updateField idx FieldKeyword (withKeywords cmc (DI.docFieldKeys idx) ks p)
      Nothing -> idx

extractTags :: Page -> [TagId]
extractTags p = concatMap expandSubTags $ pageTags p
  where expandSubTags = map segmentsToTag . tagSubSegments

-- mkPageRelPath :: FP.FilePath -> FP.FilePath -> T.Text
-- mkPageRelPath top pgPath =
--   T.pack $ FP.makeRelative top pgPath



mkField :: (Ord keyRaw) =>
           field ->
           (keyRaw -> key) ->
           (doc -> [keyRaw]) ->
           DI.Field doc field key
mkField fld mkKey extract = DI.Field fld (\doc -> map mkKey $ nodup $ extract doc)


