module Kiwi.Controller
  ( servePage
  , serveBrowseAll
  , serveSearch
  , serveEditPage
  , serveLogin
  , logUserIn
  , logUserOut
  , serveNotFound
  , withLogin
  , ifAdmin
  ) where

import           Control.Monad.Reader
import qualified Data.Map.Strict as M
import           Data.Maybe (mapMaybe)
import qualified Data.SearchEngine as SE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified Network.HTTP.Types.Status as Net
import qualified System.FilePath as FP
import qualified System.Process as Proc
import           Web.Scotty.Trans (formParam, redirect, status, html)

import           Kiwi.Types (ActM)
import qualified Kiwi.Types as K
import qualified Kiwi.View as V
import           Kiwi.Utils (getDB, getSessions, asksK, tagsListSubSegments, 
                             filterRootTags, fastListDiff)
import qualified Kiwi.Utils as U
import qualified Utils.DocIndex as DI

servePage :: K.PageUID -> ActM ()
servePage pgId = do
  db <- getRestrictedDB
  case findPage db pgId of
    Just p -> do
      let backLinkPages = DI.query (K.pagesIndex db) (U.qOnly K.FieldLink (K.pageUID p)) 
      r <- V.renderPage p backLinkPages
      html r
    Nothing -> serveNotFound


-- | Browse page: the pages matching the request, and links to narrow it
-- (next tags, sources, custom fields keys) or to remove one of its keys.
serveBrowseAll :: [(T.Text, T.Text)] -> ActM ()
serveBrowseAll params = do
  db <- getRestrictedDB
  case U.decodeReq (K.customMetaConfig db) params of
    Left err -> serveBadRequest err
    Right req -> metaIndex db req >>= html
  where
    metaIndex db req =
      let idx = K.pagesIndex db
          subset = if M.null req then idx
                   else DI.narrow idx $ U.reqToQuery req
          reqKeySet f = M.findWithDefault S.empty f req
          reqKeys = S.toList . reqKeySet
          -- tags specific handling
          allTagsSegs = tagsListSubSegments $ K.fromKeys $
                        DI.getFieldKeys subset K.FieldTag
          queryTagsSegs = tagsListSubSegments $ K.fromKeys $ reqKeys K.FieldTag
          nextTags = filterRootTags $ fastListDiff allTagsSegs queryTagsSegs
          tags = (K.FieldTag, map (K.KeyText . U.segmentsToTag) nextTags)
          -- sources specific handling
          sources = (K.FieldSource, DI.getFieldKeys subset K.FieldSource)
          -- custom fields
          fieldsAndKeys = map (fks subset) $ relevantFields subset (K.customMetaConfig db)
          -- menu construction
          menu = tags : (filterOut (sources : fieldsAndKeys))
          nextStep = map (\(f, keys) -> (f, map (nextStepLink f) keys)) $ menu
          -- keys already in the request are not proposed again
          filterOut = mapMaybe $ \(f, ks) ->
            case filter (`S.notMember` reqKeySet f) ks of
              [] -> Nothing
              a  -> Just (f, a)
          -- keys of the request, with a link to remove them
          active = [ (f, keyLabel f k, U.browseUrl $ U.removeFromReq req (f,k))
                   | (f, ks) <- M.toList req, k <- S.toList ks ]
          -- produce a label and a link to a narrower request
          nextStepLink f k = (nextStepLabel f k, U.browseUrl $ U.addToReq req (f,k))
      in V.renderBrowseAll nextStep active $ DI.documentsList subset
      where fks idx f = (f, DI.getFieldKeys idx f)

    nextStepLabel K.FieldTag (K.KeyText tag) = case U.tagToSegments tag of
      (t:[]) -> t
      segs      -> T.concat ["> ", last segs]
    nextStepLabel f k = keyLabel f k

    keyLabel K.FieldTag (K.KeyText tag) = U.prettyTag tag
    keyLabel _ k = U.showKeyLabel k

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
      pages = mapMaybe (findPage db) matches
    in case pages of
         (p:[]) -> redirect (TL.fromStrict $ U.pageUrl p)  -- un seul résultat, on y va
         _      -> do r <- V.renderSearchResults pages
                      html r


serveEditPage :: Maybe (FP.FilePath, [String]) -> K.PageUID -> ActM ()
serveEditPage editorM pgId =
  case editorM of
    Nothing -> serveNotFound
    Just (cmd, args) -> do
      db <- getDB
      case findPage db pgId of
        Nothing -> serveNotFound
        Just p -> do
          _ <- liftIO $ Proc.spawnProcess cmd $ args ++ [K.pageAbsoluteFSPath p]
          html "ok"
      

serveLogin :: ActM ()
serveLogin = do
  r <- V.renderLoginForm False
  html r

-- | On failure, the login form is displayed again with an error message
logUserIn :: ActM ()
logUserIn = do
  name <- formParam "username"
  pass <- formParam "password"
  accounts <- asksK K.accounts
  case U.verifyLogin accounts name pass of
    Nothing -> do
      r <- V.renderLoginForm True
      status Net.status401
      html r
    Just (user, groups) -> do
      key <- liftIO $ U.randomText 128
      now <- liftIO $ Time.getCurrentTime
      U.modifySessions $ M.insert key (user, groups, now)
      U.setKiwiCookie key
      redirect "/"


logUserOut :: ActM ()
logUserOut = do
  U.deleteKiwiCookie
  authM <- U.getKiwiCookie
  case authM of
    Nothing -> html "" -- TODO
    Just auth -> do
      U.modifySessions $ M.delete auth
      redirect "/"


serveNotFound :: ActM ()
serveNotFound = do
  r <- V.renderNotFound
  status Net.status404
  html r


serveBadRequest :: T.Text -> ActM ()
serveBadRequest err = do
  r <- V.renderBadRequest err
  status Net.status400
  html r


serveForbidden :: ActM ()
serveForbidden = do
  r <- V.renderForbidden
  status Net.status401
  html r


findPage :: K.PagesDB -> K.PageUID -> Maybe K.Page
findPage db pgId = DI.findDoc (K.pagesIndex db) pgId 


-- | Set login from session cookie, if the session exists and has not expired
withLogin :: ActM a -> ActM a
withLogin run = do
  authM <- U.getKiwiCookie
  sess <- getSessions
  now <- liftIO $ Time.getCurrentTime
  let login' = case authM of
        Nothing -> Nothing
        Just auth -> M.lookup auth sess >>= \l ->
          if U.isSessionValid now l then Just l else Nothing
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
  
