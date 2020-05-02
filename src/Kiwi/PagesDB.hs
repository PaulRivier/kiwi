-- {-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Kiwi.PagesDB where

import           Data.List (find)
import qualified Data.Map.Strict as M
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
import           Kiwi.Utils (traverseDir, tagSubSegments, segmentsToTag, nodup)
import qualified Utils.DocIndex as DI

updatePagesDB :: PagesDB ->  IO PagesDB
updatePagesDB db =
  let top = pagesDir db
      pages = DI.store $ pagesIndex db
      lu = lastUpdate db
      md = defaultMeta db
      cmc = customMetaConfig db
  in do
    dbOK <- getCondition
    currentTime <- C.getCurrentTime
    case dbOK  of
      True -> return db   -- pas de modification
      False -> do         -- modifications à trouver
        (modified, removed) <- getModifiedAndRemoved top pages md cmc lu
        let pagesIndexClean = DI.removeMany (pagesIndex db) (S.toList removed)
        let newPagesIndex = DI.insertMany pagesIndexClean modified
        let updSearchEngine = getUpdSearchEngine (searchEngine db) modified removed
        return $ db { lastUpdate = currentTime
                    , pagesIndex = newPagesIndex
                    , searchEngine = updSearchEngine }
  where
    -- TODO : gérer les conditions de rechargement
    getCondition = return False

    getModifiedAndRemoved top pages md cmc date = do
      fsPaths <- traverseDir top (\(x:_) -> x /= '.') -- pas de dossier caché
      fsPathsAndMTime <- mapM addMTime fsPaths
      let updatedPaths = map fst $ filter (\(_,mt) -> mt > date) fsPathsAndMTime
      newPages <- mapM (loadPageIO top md cmc) updatedPaths
      -- pages supprimées
      let pgIdFromPaths = map (pageIdFromFilePath top) fsPaths
      let missing = S.difference (M.keysSet pages) (S.fromList pgIdFromPaths)
      return (newPages, missing)

    addMTime path = do
      mt <- D.getModificationTime path
      return (path, mt)

    getUpdSearchEngine se modified removed =
        insertDocs modified $ foldr deleteDoc se (S.toList removed)




emptyPagesDB :: FP.FilePath -> MetaData -> [CustomMetaConfig] -> PagesDB
emptyPagesDB dir md cmc =
    let oldDate = C.UTCTime (Cal.fromGregorian 2000 1 1) (C.secondsToDiffTime 0)
    in PagesDB { pagesDir = dir
               , defaultMeta = md
               , customMetaConfig = cmc
               , lastUpdate = oldDate
               , pagesIndex = DI.init pageId (pageExtractKeys cmc)
               , searchEngine = initPageSearchEngine (metaLang md) }


pageExtractKeys :: [CustomMetaConfig] -> [DI.Field Page MetaField DocIndexKey]
pageExtractKeys cmc = [ mkField FieldTag KeyText extractTags
                      , mkField FieldAccess KeyText pageAccess
                      , mkField FieldLink KeyText pageLinks
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
  
pageIdFromFilePath :: FP.FilePath -> FP.FilePath -> T.Text
pageIdFromFilePath top pgPath =
    T.pack $ FP.dropExtension $ FP.makeRelative top pgPath



mkField :: (Ord field, Ord keyRaw, Ord key) =>
           field ->
           (keyRaw -> key) ->
           (doc -> [keyRaw]) ->
           DI.Field doc field key
mkField fld mkKey extract = DI.Field fld (\doc -> map mkKey $ nodup $ extract doc)
