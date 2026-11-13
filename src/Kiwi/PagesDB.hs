{-# LANGUAGE BangPatterns #-}

module Kiwi.PagesDB
  ( updatePagesDB
  , reloadPages
  , pagesFSDir
  , pagesCount
  , warmUpSearchEngine
  , emptyPagesDB
  , brokenLinks
  ) where

import           Control.Concurrent (forkIO, getNumCapabilities)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception (IOException, evaluate, try)
import           Control.Monad (forM_, unless, void, when)
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

import qualified Kiwi.Log as L
import           Kiwi.Search
import           Kiwi.Types
import           Kiwi.Pandoc (Loaded (..), loadPageIO, pagePath)
import           Kiwi.Tags (tagSubSegments, segmentsToTag)
import           Kiwi.Utils (traverseDir, nodup, isDotFile, isValidMdFilename, timeIO)
import qualified Utils.DocIndex as DI

-- | Folder of the pages, in a source folder
pagesFSDir :: FP.FilePath
pagesFSDir = "pages"

-- | Reloads the pages of the kiwi (see 'updatePagesDB'). The caller holds
-- 'pagesDBLock', so that two reloads (or a reload and a page save) can not
-- overwrite each other; readers are never blocked.
reloadPages :: ServerState -> IO ()
reloadPages ss = do
  db' <- readIORef (pagesDB ss)
  (db, t) <- timeIO $ updatePagesDB (logger ss) (contentDir ss) pagesFSDir db'
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
-- synchronized), are loaded. The index and the search engine get the same
-- removals and insertions.
updatePagesDB :: L.Logger -> FP.FilePath ->  FP.FilePath -> PagesDB ->  IO PagesDB
updatePagesDB logger' contentDir' pagesDir db = do
    sources <- filter (not . isDotFile) <$> D.listDirectory contentDir'
    -- dbOK <- getCondition
    let pagesDirs = map (\s -> FP.joinPath [contentDir', s, pagesDir]) sources
    -- case dbOK  of
    --   True -> return db   -- nothing to do
    --   False -> do         -- some work to do
    -- pas de dossier caché
    fsPathsRaw <- concat <$> mapM (\d -> traverseDir d (not . isDotFile)) pagesDirs
    let (fsPaths, ignoredPaths) = partition (isValidMdFilename . FP.takeFileName)
                                            fsPathsRaw
    forM_ ignoredPaths $ \p -> logger' L.Debug $ T.pack $ concat
      [ "file ignored, ", p, " : a page file name starts with a letter or a digit"
      , ", and ends with .md or .markdown" ]
    fsPathsAndMTime <- mapM addMTime fsPaths
    let toReload = [ p | (p, mt) <- fsPathsAndMTime, isModified p mt ]
        keptPaths = S.difference (S.fromList fsPaths) (S.fromList toReload)
        -- the pages of the files that are gone, or reloaded: a reloaded file
        -- may give another page id, or no page if it can not be loaded
        removedUIDs = [ pageUID pg | pg <- dbPages
                                   , not $ S.member (pageAbsoluteFSPath pg) keptPaths ]
        pagesIndexClean = DI.removeMany (pagesIndex db) removedUIDs
        searchEngineClean = foldr deleteDoc (searchEngine db) removedUIDs
    (loaded, failed) <- loadPages toReload
    let (inserted, duplicates) = resolveDuplicates pagesIndexClean loaded
    forM_ duplicates $ \((src, pId), ignored, kept) -> logger' L.Warn $ T.concat
      [ "duplicate page id ", src, ":", pId, ", ", T.pack ignored, " ignored, "
      , T.pack kept, " used" ]
    let newIndex = DI.insertMany pagesIndexClean inserted
        -- a file that is not loaded has no page in the index: it is loaded
        -- again at each reload, so these are all the errors
        errors = [ loadError p (T.pack e) | (p, e) <- failed ]
                 ++ [ loadError ignored $ T.concat
                        [ "duplicate page id ", pId, ", "
                        , T.pack (snd $ sourcePath kept), " is used" ]
                    | ((_, pId), ignored, kept) <- duplicates ]
    -- the links to missing pages are listed on the /doctor page, not logged:
    -- they are an issue of the content, not of the server
    return $ db { dbGeneration = dbGeneration db + 1
                , loadErrors = sortOn (\(src, p, _) -> (src, p)) errors
                , pagesIndex = newIndex
                , searchEngine = insertDocs inserted searchEngineClean }
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
        -- ids made from the path have no '/': this one is from the "id" field
        let pId = snd $ pageUID pg
        when (T.isInfixOf "/" pId) $ logger' L.Warn $ T.concat
          [ "page id with a '/', ", T.pack p, " : ", pId ]
      return ( [ loadedPage l | (_, Right l) <- results ]
             , [ (p, e) | (p, Left e) <- results ] )

    -- source of a file, and its path in the pages folder of the source
    sourcePath path =
      let source = fst $ pagePath contentDir' path
      in (source, FP.makeRelative (FP.joinPath [contentDir', T.unpack source, pagesDir]) path)
    loadError path msg = let (source, rel) = sourcePath path in (source, rel, msg)

    -- file may have been removed since directory listing : no mtime, it
    -- will then be reported by loadPage
    addMTime path = do
      mtE <- try $ D.getModificationTime path
      return (path, either (\(_ :: IOException) -> Nothing) Just mtE)

    loadPage :: FP.FilePath -> FP.FilePath -> IO (Either String Loaded)
    loadPage cd fullPath =
      let (source, relPath) = pagePath cd fullPath
          md = defaultMeta db
          cmc = customMetaConfig db
          sc = find (\c -> scSourceName c == source) $ sourcesConfig db
      in loadPageIO fullPath relPath source md cmc sc


-- | The loaded pages to insert in the index (without the pages of the files
-- that are not loaded again), and the duplicates ignored: (page id, ignored
-- file, used file). When several files give the same page id, the page of
-- the first file path is used, whether it is loaded or already in the index,
-- so that the same page is shown after each reload.
resolveDuplicates :: DI.DocIndex Page PageUID MetaField DocIndexKey ->
                     [Page] -> ([Page], [(PageUID, FP.FilePath, FP.FilePath)])
resolveDuplicates idx loaded =
  let byUID = M.fromListWith (flip (++)) [ (pageUID pg, [pg]) | pg <- loaded ]
      results = map resolve $ M.toList byUID
  in (concatMap fst results, concatMap snd results)
  where
    resolve (uid, news) =
      let existing = pageAbsoluteFSPath <$> DI.findDoc idx uid
          newPaths = map pageAbsoluteFSPath news
          -- not empty: there is a loaded page for each uid
          used = minimum $ maybe id (:) existing newPaths
          ignored = filter (/= used) $ maybe id (:) existing newPaths
      in ( [ pg | pg <- news, pageAbsoluteFSPath pg == used ]
         , [ (uid, p, used) | p <- ignored ] )




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
            -- , lastUpdate = oldDate
            , pagesIndex = DI.empty pageUID (pageExtractKeys cmc)
            , searchEngine = initPageSearchEngine }


pageExtractKeys :: [CustomMetaConfig] -> [DI.Field Page MetaField DocIndexKey]
pageExtractKeys cmc = [ mkField FieldSource KeyText ((:[]) . fst . pageUID)
                      , mkField FieldTag KeyText extractTags
                      , mkField FieldAccess KeyText (map fst . pageAccess)
                      , mkField FieldLink KeyPairText pageLinks
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


