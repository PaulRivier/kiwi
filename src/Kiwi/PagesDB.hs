{-# LANGUAGE BangPatterns #-}

module Kiwi.PagesDB
  ( updatePagesDB
  , reloadPages
  , pagesCount
  , warmUpSearchEngine
  , emptyPagesDB
  , brokenLinks
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
import           Kiwi.Related (relatedFeaturesOf)
import qualified Kiwi.Log as L
import           Kiwi.Meta (isValidUid, uidFromBytes, writePage)
import           Kiwi.Search
import           Kiwi.Types
import           Kiwi.Pandoc (Loaded (..), loadPageIO, pagePath)
import           Kiwi.Tags (tagSubSegments, segmentsToTag)
import           Kiwi.Utils (traverseDir, nodup, isDotFile, isValidMdFilename, timeIO)
import qualified Utils.DocIndex as DI

-- | Reloads the pages of the kiwi (see 'updatePagesDB'). The caller holds
-- 'pagesDBLock', so that two reloads (or a reload and a page save) can not
-- overwrite each other; readers are never blocked.
reloadPages :: ServerState -> IO ()
reloadPages ss = do
  db' <- readIORef (pagesDB ss)
  (db, t) <- timeIO $ updatePagesDB (logger ss) (gitCommit ss) (contentDir ss) db'
  atomicWriteIORef (pagesDB ss) db
  warmUpSearchEngine (searchEngine db)
  logger ss L.Info $ T.pack $ concat [ "pages reloaded in ", printf "%.2f" t
                                     , "s, ", pagesCount db, " pages" ]

pagesCount :: PagesDB -> String
pagesCount = show . M.size . DI.store . pagesIndex

-- | Evaluate the search engine in a thread, so that the first search after
-- a load does not wait for it
warmUpSearchEngine :: PageSearchEngine -> IO ()
warmUpSearchEngine se = void $ forkIO $ void $ evaluate $ SE.invariant se


-- | Loads the files of the pages that are new or modified since they were
-- loaded, and removes the pages of the files that are gone, or reloaded. A
-- file is reloaded when its modification time differs from the one of its
-- loaded page, so that renamed files, and files with an old date (copied,
-- synchronized), are loaded. The
-- index and the search engine get the same removals and insertions. Each
-- page gets a uid unique in its source (see 'resolveUids'): the files whose
-- uid changes are written, and committed when the boolean is true; a page
-- whose file can not be written is not loaded.
--
-- The other files of the sources get a uid (see 'resolveFiles'), and are
-- listed in 'sourceFiles'.
updatePagesDB :: L.Logger -> Bool -> FP.FilePath -> PagesDB -> IO PagesDB
updatePagesDB logger' git contentDir' db = do
    sources <- filter (not . isDotFile) <$> D.listDirectory contentDir'
    -- the pages are anywhere in the folder of their source, except in the
    -- hidden folders (".git"…) and those starting with '_'
    sourceDirs <- filterM D.doesDirectoryExist $ map (FP.combine contentDir') sources
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
    (files, fileErrors) <- resolveFiles logger' git contentDir' (sourceFiles db)
                                                 (map sourcePath servable)
    fsPathsAndMTime <- mapM addMTime fsPaths
    let toReload = [ p | (p, mt) <- fsPathsAndMTime, isModified p mt ]
        keptPaths = S.difference (S.fromList fsPaths) (S.fromList toReload)
        -- the pages of the files that are gone, or reloaded: a reloaded file
        -- may give another page id, or no page if it can not be loaded
        removedPages = [ pg | pg <- dbPages, not $ S.member (pageAbsoluteFSPath pg) keptPaths ]
        removedUIDs = map pageUID removedPages
        pagesIndexClean = DI.removeMany (pagesIndex db) removedUIDs
        searchEngineClean = foldr deleteDoc (searchEngine db) removedUIDs
    (loaded, failed) <- loadPages toReload
    -- the pages of the index keep their uid
    resolved <- resolveUids logger' contentDir'
                  (S.fromList $ map pageUID $ DI.documentsList pagesIndexClean)
                  (sortOn (pageAbsoluteFSPath . loadedPage) loaded)
    let cmc = customMetaConfig db
        texts = [ (pageAbsoluteFSPath pg, writePage cmc h (pageBody pg))
                | (pg, Just h) <- resolved ]
    written <- if null texts then return M.empty
               else M.fromList <$> writePageFiles logger' git "uid assigned to" texts
    -- the time of a written file is read again, so that it is not reloaded
    inserted <- fmap concat $ forM resolved $ \(pg, _) ->
      case M.lookup (pageAbsoluteFSPath pg) written of
        Nothing -> return [pg]
        Just (Left _) -> return []
        Just (Right ()) -> do
          mtE <- try $ D.getModificationTime (pageAbsoluteFSPath pg)
          return [either (\(_ :: IOException) -> pg) (\mt -> pg { pageMTime = mt }) mtE]
    let newIndex = DI.insertMany pagesIndexClean inserted
        -- a file that is not loaded has no page in the index: it is loaded
        -- again at each reload, so these are all the errors
        errors = [ loadError p (T.pack e) | (p, e) <- failed ]
                 ++ [ loadError p (T.pack $ "unable to write the uid of the page : " ++ e)
                    | (p, Left e) <- M.toList written ]
    -- the links to missing pages are listed on the /doctor page, not logged:
    -- they are an issue of the content, not of the server
        newEngine = insertDocs inserted searchEngineClean
    -- the keywords of the pages read again, or of every page from time to
    -- time, as they depend on the other pages
    ((keywords, full), t) <- timeIO $ do
      let r@(kw, _) = updateKeywords (DI.documentsList newIndex) removedPages inserted
                                     (pageKeywords db)
      _ <- evaluate kw
      return r
    when full $ logger' L.Info $ T.pack $ "TF-IDF finished in " ++ printf "%.2f" t ++ "s"
    -- the features of every page for the related pages, as they change
    -- with the keywords (see "Kiwi.Related")
    let features = relatedFeaturesOf (customMetaConfig db) (kwPages keywords)
                                     (DI.documentsList newIndex)
    _ <- evaluate $ M.foldl' (\n f -> n + featureNorm f) 0 features
    return db { dbGeneration = dbGeneration db + 1
              , loadErrors = sortOn (\(src, p, _) -> (src, p)) (errors ++ fileErrors)
              , sourceFiles = files
              , pagesIndex = newIndex
              , searchEngine = newEngine
              , pageKeywords = keywords
              , relatedFeatures = features }
  where
    -- TODO : gérer des conditions de rechargement
    -- getCondition = return False

    dbPages = DI.documentsList $ pagesIndex db
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
-- (that of the file, with this uid). The uid replaced, if any, goes to the
-- previous-uids.
resolveUids :: L.Logger -> FP.FilePath -> S.Set PageUID -> [Loaded] ->
               IO [(Page, Maybe MetaData)]
resolveUids _ _ _ [] = return []
resolveUids logger' contentDir' used (l : ls) = do
  let pg = loadedPage l
      path = pageAbsoluteFSPath pg
      header = loadedHeader l
      source = fst $ pagePath contentDir' path
  (uid, newHeader) <- case metaUid header of
    Just u | isValidUid u, not (S.member (source, u) used) -> return (u, Nothing)
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
      return (u, Just header { metaUid = Just u, metaPreviousUids = previousM })
  let pg' = pg { pageUID = (source, uid)
               , pagePreviousUids = maybe (pagePreviousUids pg)
                                          (fromMaybe [] . metaPreviousUids) newHeader }
  ((pg', newHeader) :) <$> resolveUids logger' contentDir' (S.insert (source, uid) used) ls
  where
    newUid source = do
      u <- uidFromBytes <$> (CR.getRandomBytes 10 :: IO BS.ByteString)
      if S.member (source, u) used then newUid source else return u

-- | Writes these texts of page files (see 'writePageFile'), and the result
-- for each file. When the boolean is true (git-commit), the files that
-- differ from their last commit are first committed as they are, and the
-- files written are then committed, with the message "Kiwi: WHAT N pages".
-- The failures are logged, as errors for the writes (an issue of the system).
writePageFiles :: L.Logger -> Bool -> T.Text -> [(FP.FilePath, T.Text)] ->
                  IO [(FP.FilePath, Either String ())]
writePageFiles logger' git what texts = do
  when git $ commitPaths logger' "Kiwi: pre-update commit of" (count "pages" texts) (map fst texts)
  results <- forM texts $ \(p, t) ->
    (p,) . first (\(e :: IOException) -> show e) <$> try (writePageFile p t)
  forM_ [ (p, e) | (p, Left e) <- results ] $ \(p, e) ->
    logger' L.Error $ T.pack $ concat [ "unable to write the file ", p, " : ", e ]
  let ok = [ p | (p, Right ()) <- results ]
  logger' L.Info $ T.concat [ what, " ", T.pack (show $ length ok), " pages" ]
  when (git && not (null ok)) $
    commitPaths logger' (T.concat ["Kiwi: ", what]) (count "pages" ok) ok
  return results

-- | "N WHAT", N being the number of items
count :: T.Text -> [a] -> T.Text
count what xs = T.concat [T.pack (show $ length xs), " ", what]

-- | Commits these files, with the message "PREFIX COUNT" (see
-- 'Git.commitFiles'); the failures are logged
commitPaths :: L.Logger -> T.Text -> T.Text -> [FP.FilePath] -> IO ()
commitPaths logger' prefix n paths = do
  rs <- Git.commitFiles paths "Kiwi" (T.concat [prefix, " ", n])
  let committed = length [ () | (_, Right Git.Committed) <- rs ]
  when (committed > 0) $ logger' L.Info $ T.concat
    [ prefix, ": ", T.pack (show committed), " files committed" ]
  forM_ (nodup [ e | (_, Left e) <- rs ]) $ \e ->
    logger' L.Warn $ T.pack $ concat [ "files not committed: ", e ]


-- | The files of the sources that are not pages, given as (source, path
-- from the top of the source), each one with a uid unique in its source
-- (see "Kiwi.Files"): a file keeps the uid of its name when it is not used,
-- else it gets a new one, and is renamed. The files of the given table (the
-- previous load) that keep their path are considered first, then the others
-- in the order of their paths: a copy of a file gets a new uid, not the
-- file. The files renamed are committed when the boolean is true
-- (git-commit), first as they are, then renamed, as for the pages (see
-- 'writePageFiles'). Returns the table of the files, and the files that
-- could not be renamed (they have no uid), as load errors.
resolveFiles :: L.Logger -> Bool -> FP.FilePath -> M.Map (T.Text, T.Text) FP.FilePath ->
                [(T.Text, FP.FilePath)] ->
                IO (M.Map (T.Text, T.Text) FP.FilePath, [(T.Text, FP.FilePath, T.Text)])
resolveFiles logger' git contentDir' previous files = do
  assigned <- assign S.empty $ known ++ others
  let renames = [ x | x@(_, path, new) <- assigned, path /= new ]
      full (source, _) path = FP.joinPath [contentDir', T.unpack source, path]
  when (git && not (null renames)) $
    commitPaths logger' "Kiwi: pre-update commit of" (count "files" renames)
                [ full k p | (k, p, _) <- renames ]
  results <- forM renames $ \(k, path, new) -> do
    let target = full k new
    exists <- D.doesPathExist target
    r <- if exists then return $ Left "the new name is already used"
         else first (\(e :: IOException) -> show e) <$> try (D.renamePath (full k path) target)
    case r of
      Left e -> logger' L.Error $ T.pack $ concat
                  [ "unable to rename the file ", full k path, " : ", e ]
      Right () -> logger' L.Debug $ T.concat [ "uid assigned, ", T.pack (full k new) ]
    return (k, path, new, r)
  let ok = [ (k, path, new) | (k, path, new, Right ()) <- results ]
  unless (null renames) $ logger' L.Info $ T.concat
    [ "uid assigned to ", T.pack (show $ length ok), " files" ]
  when (git && not (null ok)) $
    commitPaths logger' "Kiwi: uid assigned to" (count "files" ok)
                (concat [ [full k path, full k new] | (k, path, new) <- ok ])
  let failed = S.fromList [ k | (k, _, _, Left _) <- results ]
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
brokenLinks :: DI.DocIndex Page PageUID MetaField DocIndexKey -> [(Page, PageUID)]
brokenLinks idx =
  [ (pg, uid) | pg <- sortOn pageAbsoluteFSPath $ DI.documentsList idx
              , uid <- nodup $ pageLinks pg
              , isNothing $ DI.findDoc idx uid ]


emptyPagesDB :: MetaData -> [CustomMetaConfig] -> [SourceConfig] -> PagesDB
emptyPagesDB md cmc sc =
    -- let oldDate = C.UTCTime (Cal.fromGregorian 2000 1 1) (C.secondsToDiffTime 0)
    PagesDB { defaultMeta = md
            , customMetaConfig = cmc
            , sourcesConfig = sc
            , dbGeneration = 0
            , loadErrors = []
            , sourceFiles = M.empty
            -- , lastUpdate = oldDate
            , pagesIndex = DI.empty pageUID (pageExtractKeys cmc)
            , searchEngine = initPageSearchEngine
            , pageKeywords = emptyKeywords
            , relatedFeatures = M.empty }


pageExtractKeys :: [CustomMetaConfig] -> [DI.Field Page MetaField DocIndexKey]
pageExtractKeys cmc = [ mkField FieldSource KeyText ((:[]) . fst . pageUID)
                      , mkField FieldTag KeyText extractTags
                      , mkField FieldAccess KeyText (map fst . pageAccess)
                      , mkField FieldLink KeyPairText pageLinks
                      , mkField FieldFile KeyPairText (\p -> map (fst (pageUID p),) (pageFiles p))
                      , mkField FieldLang KeyLang ((:[]) . pageLang)
                      , mkField FieldArchive KeyBool ((:[]) . pageArchive)
                      ] ++
                      map mkCustomField cmc
  where
    mkCustomField c =
      let fld = (FieldCustom (cmcName c))
          keys = \p -> fromMaybe [] $ findCmv (cmcName c) p
      in DI.Field fld keys
    findCmv n p = cmdKeys <$> find (\cmd -> cmdName cmd == n) (pageCustomMeta p)

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


