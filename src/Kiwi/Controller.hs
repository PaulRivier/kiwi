{-# LANGUAGE OverloadedStrings #-}

module Kiwi.Controller  where

import           Control.Monad.Reader
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import qualified Data.SearchEngine as SE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified Network.HTTP.Types.Status as Net
import qualified System.FilePath as FP
import qualified System.Process as Proc
import           Web.Scotty.Trans (param, redirect, status, html, liftAndCatchIO)

import           Kiwi.Types (ActM)
import qualified Kiwi.Types as K
import qualified Kiwi.View as V
import           Kiwi.Utils (getDB, getSessions, asksK, tagsListSubSegments, 
                             filterRootTags, nodup, fastListDiff)
import qualified Kiwi.Utils as U
import qualified Utils.DocIndex as DI

servePage :: T.Text -> ActM ()
servePage pgId = do
  db <- getRestrictedDB
  case findPage db pgId of
    Just p -> do
      let backLinkPages = DI.query (K.pagesIndex db) (U.qOnly K.FieldLink (K.pageId p)) 
      r <- V.renderPage p backLinkPages
      html r
    Nothing -> serveNotFound

  
serveTagged :: T.Text -> ActM ()
serveTagged tags' = do
  db <- getRestrictedDB
  r <- if tags' == "*" then taggedIndex db
       else taggedWith db (U.splitTextEsc ',' tags')
  html r
  where
    taggedIndex db =
      let rootTags = filterRootTags $ tagsListSubSegments $ nodup $
                     concat (map K.pageTags $ DI.documentsList (K.pagesIndex db))
      in V.renderTagged [] rootTags $ DI.documentsList (K.pagesIndex db)
    taggedWith db query =
      let idx = K.pagesIndex db
          subset = DI.narrow idx $ U.qAll K.FieldTag query
          matchingPages = DI.documentsList subset
          matchesAllTagsSegs = tagsListSubSegments $ K.fromKeys $
                               DI.getFieldKeys subset K.FieldTag
          querySegs = tagsListSubSegments query
          nextTags = filterRootTags $ fastListDiff matchesAllTagsSegs querySegs
      in V.renderTagged query nextTags matchingPages




type BrowseAllReq = M.Map K.MetaField [K.DocIndexKey]

-- TODO : this works but should be improved and clarified
serveBrowseAll :: T.Text -> ActM ()
serveBrowseAll r' = do
  db <- getRestrictedDB
  let req = U.decodeReq r'
  response <- metaIndex db req
  html response
  where
    metaIndex db req =
      let idx = K.pagesIndex db
          subset = if M.null req then idx
                   else DI.narrow idx $ U.reqToQuery req
          -- tags specific handling
          allTagsSegs = tagsListSubSegments $ K.fromKeys $
                        DI.getFieldKeys subset K.FieldTag
          queryTagsSegs = tagsListSubSegments $ K.fromKeys $
                          M.findWithDefault [] K.FieldTag req
          nextTags = filterRootTags $ fastListDiff allTagsSegs queryTagsSegs
          tags = (K.FieldTag, map (K.KeyText . U.segmentsToTag) nextTags)
          -- custom fields
          fieldsAndKeys = map (fks subset) $ relevantFields subset (K.customMetaConfig db)
          nextStep = map (\(f, keys) -> (f, map (nextStepLink req f) keys))  $
                     tags : filterOut req fieldsAndKeys
      in V.renderBrowseAll nextStep $ DI.documentsList subset
      where fks idx f = (f, DI.getFieldKeys idx f)

    -- produce a label and a link to a query
    nextStepLink :: BrowseAllReq -> K.MetaField -> K.DocIndexKey -> (T.Text, T.Text)
    nextStepLink r f k = let nextReq = U.addToReq r (f,k) 
                             link = U.encodeReq nextReq
                             label = case f of
                               K.FieldTag -> makeTagLabel k
                               _ -> U.showKeyLabel k
                         in (label, link)

    makeTagLabel (K.KeyText tag) = case U.tagToSegments tag of
      (t:[]) -> t
      segs      -> T.concat ["> ", last segs]
    makeTagLabel _ = "Not a tag"

    filterOut :: BrowseAllReq -> [(K.MetaField, [K.DocIndexKey])] ->
                 [(K.MetaField, [K.DocIndexKey])]
    filterOut req fks = catMaybes $ map (notInReq) fks
      where notInReq (f, ks) = case fastListDiff ks (M.findWithDefault [] f req) of
              [] -> Nothing
              a  -> Just (f, a)

    relevantFields idx cmcs =
      let cardinal = DI.cardinal idx
          relevants = filter (ratioReached cardinal) cmcs
      in fmap (K.FieldCustom . K.cmcName) relevants
      where
        ratioReached total cmc
          | K.cmcThreshold cmc > 1  = False -- never reached
          | K.cmcThreshold cmc <= 0 = True  -- always reached
          | otherwise =
              let fieldCardinal = S.size $ DI.queryIdSet idx $
                                  U.qHasField (K.FieldCustom $ K.cmcName cmc)
                  ratio = fromIntegral fieldCardinal / fromIntegral total
              in ratio >= K.cmcThreshold cmc


serveSearch :: T.Text -> ActM ()
serveSearch query = do
  db <- getRestrictedDB
  let matches = SE.query (K.searchEngine db) (T.words query)
      pages = catMaybes $ map (findPage db) matches
    in case pages of
         (p:[]) -> redirect (TL.fromStrict $ U.pageUrl p)  -- un seul résultat, on y va
         _      -> do r <- V.renderSearchResults pages
                      html r


serveEditPage :: Maybe (FP.FilePath, [String]) -> T.Text -> ActM ()
serveEditPage editorM pgId =
  case editorM of
    Nothing -> serveNotFound
    Just (cmd, args) -> do
      db <- getDB
      case findPage db pgId of
        Nothing -> serveNotFound
        Just p -> let filePath = FP.joinPath [ K.pagesDir db, K.pageFSPath p ]
                  in do
          _ <- liftAndCatchIO $ Proc.spawnProcess cmd (args ++ [filePath])
          html "ok"
      

serveLogin :: ActM ()
serveLogin = do
  r <- V.renderLoginForm
  html r

logUserIn :: ActM ()
logUserIn = do
  name <- param "username"
  pass <- param "password"
  accounts <- asksK K.accounts
  kd <- asksK K.kiwiDir
  case U.verifyLogin accounts name pass of
    Nothing -> serveNotFound -- TODO : proper response
    Just (user, groups) -> do
      sess <- getSessions
      key <- liftAndCatchIO $ U.randomText 128
      now <- liftAndCatchIO $ Time.getCurrentTime
      let newSess = M.insert key (user, groups, now) sess
      U.updateSessions newSess
      liftAndCatchIO $ U.dumpSessions newSess $ FP.combine kd "_sessions"
      -- setHeader "Set-Cookie" (TL.fromStrict key)
      U.setKiwiCookie key
      redirect "/page/Home"


logUserOut :: ActM ()
logUserOut = do
  U.deleteKiwiCookie
  sess <- getSessions
  authM <- U.getKiwiCookie
  kd <- asksK K.kiwiDir
  case authM of
    Nothing -> html "" -- TODO
    Just auth -> do
      let newSess = M.delete auth sess
      U.updateSessions newSess
      liftAndCatchIO $ U.dumpSessions newSess $ FP.combine kd "_sessions"
      redirect "/"


serveNotFound :: ActM ()
serveNotFound = do
  r <- V.renderNotFound
  status Net.status404
  html r


serveForbidden :: ActM ()
serveForbidden = do
  r <- V.renderForbidden
  status Net.status401
  html r


findPage :: K.PagesDB -> T.Text -> Maybe K.Page
findPage db pgId = DI.findDoc (K.pagesIndex db) pgId 


withLogin :: ActM a -> ActM a
withLogin run = do
  authM <- U.getKiwiCookie
  sess <- getSessions
  let login' = case authM of
        Nothing -> Nothing
        Just auth -> M.lookup auth sess
  local (setLogin login') run
  where
    setLogin l ss = ss { K.login = l }


ifAdmin :: ActM () -> ActM ()
ifAdmin a = withLogin $ do
  adm <- U.isAdmin
  if adm then a
    else serveForbidden


getRestrictedDB :: ActM K.PagesDB
getRestrictedDB = do
  db <- getDB
  loginM <- asksK K.login
  isAdmin <- U.isAdmin
  return $ case loginM of
    Nothing -> restrictDB db ["public" :: T.Text ]
    Just (_,g,_) -> if isAdmin then db else restrictDB db ("public":g)
  where
    restrictDB db groups =
      let newIdx = DI.narrow (K.pagesIndex db) (U.qAny K.FieldAccess groups)
      in db { K.pagesIndex = newIdx }
  
