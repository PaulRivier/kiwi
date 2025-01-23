-- {-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Kiwi.PagesDB where

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

import           Kiwi.Search
import           Kiwi.Types
import           Kiwi.Pandoc (loadPageIO)
import           Kiwi.Utils (traverseDir, tagSubSegments, segmentsToTag,
                             nodup, concatMapM, isDotFile, isValidMdFilename)
import qualified Utils.DocIndex as DI

updatePagesDB :: FP.FilePath ->  FP.FilePath -> PagesDB ->  IO PagesDB
updatePagesDB contentDir' pagesDir db =
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
        fsPathsRaw <- concatMapM (\d -> traverseDir d (not . isDotFile)) pagesDirs
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

    getNewOrMod cd date fsPaths = do
      fsPathsAndMTime <- mapM addMTime fsPaths
      let updatedPaths = map fst $ filter (\(_,mt) -> mt > date) fsPathsAndMTime
      mapM (loadPage cd) updatedPaths

    getRemovedUIDs dbPages fsPaths =
      let fsPathsSet = S.fromList fsPaths
          notInPaths = \pg -> not $ S.member (pageAbsoluteFSPath pg) fsPathsSet
          missing = filter notInPaths dbPages
      in map pageUID missing

    addMTime path = do
      mt <- D.getModificationTime path
      return (path, mt)

    loadPage :: FP.FilePath -> FP.FilePath -> IO Page
    loadPage cd fullPath =
      let splitPath = FP.splitDirectories $ FP.makeRelative cd fullPath
          source = T.pack $ head splitPath
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
               , searchEngine = initPageSearchEngine (metaLang md) }


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
  
mkPageRelPath :: FP.FilePath -> FP.FilePath -> T.Text
mkPageRelPath top pgPath =
  T.pack $ FP.makeRelative top pgPath



mkField :: (Ord field, Ord keyRaw, Ord key) =>
           field ->
           (keyRaw -> key) ->
           (doc -> [keyRaw]) ->
           DI.Field doc field key
mkField fld mkKey extract = DI.Field fld (\doc -> map mkKey $ nodup $ extract doc)


