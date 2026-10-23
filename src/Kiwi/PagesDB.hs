{-# LANGUAGE BangPatterns #-}

module Kiwi.PagesDB
  ( updatePagesDB
  , emptyPagesDB
  ) where

import           Control.Exception (IOException, try)
import           Control.Monad (forM_)
import           Data.List (find)
-- import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.SearchEngine (insertDocs, deleteDoc)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Time.Calendar as Cal
import qualified Data.Time.Clock as C
import qualified System.Directory as D
import qualified System.FilePath as FP

import qualified Kiwi.Log as L
import           Kiwi.Search
import           Kiwi.Types
import           Kiwi.Pandoc (loadPageIO)
import           Kiwi.Utils (traverseDir, tagSubSegments, segmentsToTag,
                             nodup, isDotFile, isValidMdFilename)
import qualified Utils.DocIndex as DI

updatePagesDB :: L.Logger -> FP.FilePath ->  FP.FilePath -> PagesDB ->  IO PagesDB
updatePagesDB logger' contentDir' pagesDir db =
  let dbPages = DI.documentsList $ pagesIndex db
      lu = lastUpdate db
  in do
    sources <- filter (not . isDotFile) <$> D.listDirectory contentDir'
    dbOK <- getCondition
    currentTime <- C.getCurrentTime
    let pagesDirs = map (\s -> FP.joinPath [contentDir', s, pagesDir]) sources
    case dbOK  of
      True -> return db   -- nothing to do
      False -> do         -- some work to do
         -- pas de dossier caché
        fsPathsRaw <- concat <$> mapM (\d -> traverseDir d (not . isDotFile)) pagesDirs
        let fsPaths = filter (isValidMdFilename . FP.takeFileName) fsPathsRaw
        newOrMod <- getNewOrMod contentDir' lu fsPaths
        let removedUIDs = getRemovedUIDs dbPages fsPaths
        let pagesIndexClean = DI.removeMany (pagesIndex db) removedUIDs
        let newPagesIndex = DI.insertMany pagesIndexClean newOrMod
        let updSearchEngine = getUpdSearchEngine (searchEngine db) newOrMod removedUIDs
        return $ db { lastUpdate = currentTime
                    , pagesIndex = newPagesIndex
                    , searchEngine = updSearchEngine }
  where
    -- TODO : gérer des conditions de rechargement
    getCondition = return False

    -- pages that can not be loaded are ignored, with a warning
    getNewOrMod cd date fsPaths = do
      fsPathsAndMTime <- mapM addMTime fsPaths
      let updatedPaths = map fst $ filter (\(_,mt) -> maybe True (> date) mt)
                                          fsPathsAndMTime
      results <- mapM (\p -> (p,) <$> loadPage cd p) updatedPaths
      forM_ [ (p, e) | (p, Left e) <- results ] $ \(p, e) ->
        logger' L.Warn $ T.pack $ concat ["page ignored, ", p, " : ", e]
      -- the lang may have been detected from the content
      forM_ [ (p, pg) | (p, Right pg) <- results ] $ \(p, pg) ->
        logger' L.Debug $ T.pack $ concat ["page loaded, ", p, " : ", show (pageLang pg)]
      return [ pg | (_, Right pg) <- results ]

    getRemovedUIDs dbPages fsPaths =
      let fsPathsSet = S.fromList fsPaths
          notInPaths = \pg -> not $ S.member (pageAbsoluteFSPath pg) fsPathsSet
          missing = filter notInPaths dbPages
      in map pageUID missing

    -- file may have been removed since directory listing : no mtime, it
    -- will then be reported by loadPage
    addMTime path = do
      mtE <- try $ D.getModificationTime path
      return (path, either (\(_ :: IOException) -> Nothing) Just mtE)

    loadPage :: FP.FilePath -> FP.FilePath -> IO (Either String Page)
    loadPage cd fullPath =
      let splitPath = FP.splitDirectories $ FP.makeRelative cd fullPath
          source = T.pack $ concat $ take 1 splitPath
          relPath = FP.dropExtension $ FP.joinPath $ drop 2 $ splitPath
          md = defaultMeta db
          cmc = customMetaConfig db
          sc = find (\c -> scSourceName c == source) $ sourcesConfig db
      in loadPageIO fullPath relPath source md cmc sc

    getUpdSearchEngine se modified removed =
        insertDocs modified $ foldr deleteDoc se removed




emptyPagesDB :: MetaData -> [CustomMetaConfig] -> [SourceConfig] -> PagesDB
emptyPagesDB md cmc sc =
    let oldDate = C.UTCTime (Cal.fromGregorian 2000 1 1) (C.secondsToDiffTime 0)
    in PagesDB { defaultMeta = md
               , customMetaConfig = cmc
               , sourcesConfig = sc
               , lastUpdate = oldDate
               , pagesIndex = DI.init pageUID (pageExtractKeys cmc)
               , searchEngine = initPageSearchEngine (baseLang $ metaLang md) }


pageExtractKeys :: [CustomMetaConfig] -> [DI.Field Page MetaField DocIndexKey]
pageExtractKeys cmc = [ mkField FieldSource KeyText ((:[]) . fst . pageUID)
                      , mkField FieldTag KeyText extractTags
                      , mkField FieldAccess KeyText pageAccess
                      , mkField FieldLink KeyPairText pageLinks
                      , mkField FieldLang KeyLang ((:[]) . pageLang)
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


