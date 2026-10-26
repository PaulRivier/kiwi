module Kiwi.Controller
  ( servePage
  , serveBrowseAll
  , serveSearch
  , serveMetadata
  , serveReadme
  , serveCommonMark
  , serveEditPage
  , serveLogin
  , logUserIn
  , logUserOut
  , serveNotFound
  , withLogin
  , withContext
  , ifAdmin
  ) where

import           Control.Monad (forM_)
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import           Data.List (isPrefixOf, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import           Data.Ord (Down (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Time as Time
import qualified Data.Tree as Tree
import qualified Network.HTTP.Types.Status as Net
import qualified System.FilePath as FP
import qualified System.Process as Proc
import qualified Text.Pandoc.Definition as PD
import           Web.Scotty.Trans (formParam, queryParams, redirect, status, html)

import           Kiwi.Types (ActM)
import qualified Kiwi.Log as L
import qualified Kiwi.DefaultPages as DP
import           Kiwi.Pandoc (pageDoc, embeddedDoc)
import           Kiwi.Search (searchPages, queryTerms, normalizeText)
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
      cd <- asksK K.contentDir
      -- the page was parsed when loaded, so this should not fail
      doc <- orRenderError (K.pageAbsoluteFSPath p) $ pageDoc cd p
      let backLinkPages = DI.query (K.pagesIndex db) (U.qOnly K.FieldLink (K.pageUID p))
      r <- V.renderPage p doc backLinkPages
      html r
    Nothing -> serveNotFound

-- | The document, or a document telling that it can not be rendered
orRenderError :: FP.FilePath -> Either String PD.Pandoc -> ActM PD.Pandoc
orRenderError _ (Right doc) = return doc
orRenderError path (Left e) = do
  logMsg L.Warn $ T.pack $ concat ["unable to render ", path, " : ", e]
  return $ PD.Pandoc mempty [PD.Para [PD.Str "Unable to render this page."]]


-- | README of kiwi, embedded in the binary
serveReadme :: ActM ()
serveReadme = serveEmbeddedDoc "README.md" False "README" DP.readmePage

-- | CommonMark cheatsheet, embedded in the binary
serveCommonMark :: ActM ()
serveCommonMark = serveEmbeddedDoc "CommonMark.md" True "CommonMark" DP.commonmarkDocPage

-- | A file embedded in kiwi, with or without metadata header. The title of
-- its header is displayed, the default title is only the one of the tab.
serveEmbeddedDoc :: FP.FilePath -> Bool -> T.Text -> BS.ByteString -> ActM ()
serveEmbeddedDoc name withHeader defaultTitle content = do
  let (titleM, docE) = case embeddedDoc name withHeader (TE.decodeUtf8Lenient content) of
        Right (t, d) -> (t, Right d)
        Left e       -> (Nothing, Left e)
  doc <- orRenderError name docE
  r <- V.renderDoc (fromMaybe defaultTitle titleM) titleM doc
  html r


-- | Metadata of the pages, by source (all the sources with pages visible to
-- the user, whatever the sources context, but in the archives or not): the
-- keys of the tags, langs, custom fields and, for admins, access groups. Each
-- key has its number of pages, and a link to browse them in its source.
serveMetadata :: ActM ()
serveMetadata = do
  db <- getRestrictedDB
  ctx <- asksK K.context
  admin <- U.isAdmin
  let idx = K.pagesIndex db
      sources = sortOn textOrder [ s | K.KeyText s <- DI.getFieldKeys idx K.FieldSource ]
      fields = [K.FieldTag, K.FieldLang]
               ++ map (K.FieldCustom . K.cmcName) (K.customMetaConfig db)
               ++ [K.FieldAccess | admin]
      section s =
        let sub = DI.narrow idx (U.qOnly K.FieldSource s)
        in (s, [ (f, map (fmap $ entry s sub f) $ keysForest f keys) | f <- fields
               , let keys = DI.getFieldKeys sub f, not (null keys) ])
      entry s sub f k = V.MetaEntry
        { V.metaLabel = keyLabel f k
        , V.metaCopy  = keyCopy f k
        , V.metaLink  = U.browseUrl ctx $ U.addToReq (U.sourcesReq $ S.singleton s) (f, k)
        , V.metaCount = S.size $ DI.lookupIndex sub f k }
  V.renderMetadata (map section sources) >>= html
  where
    -- a sub-tag shows its last segment, below its parent
    keyLabel K.FieldTag (K.KeyText t) = case reverse (U.tagToSegments t) of
      seg : _ -> seg
      []      -> t
    keyLabel _ k = U.showKeyLabel k
    keyCopy K.FieldTag (K.KeyText t) = U.prettyTag t
    keyCopy _ k = U.showKeyLabel k
    -- texts without case nor accents, tags segment by segment (the raw
    -- segment breaks ties, so that a tag is followed by its sub-tags), the
    -- other keys (numbers, dates...) in their natural order
    keysForest K.FieldTag keys = tagsForest $ sortOn (map segOrder . fst)
      [ (U.tagToSegments t, k) | k@(K.KeyText t) <- keys ]
    keysForest _ keys = [ Tree.Node k [] | k <- sortOn keyOrder keys ]
    segOrder seg = (textOrder seg, seg)
    keyOrder (K.KeyText t) = Left (textOrder t, t)
    keyOrder k = Right k
    textOrder = normalizeText . T.toLower

-- | Tags sorted segment by segment, as a forest of tags and sub-tags. The
-- parents are among the tags, since 'K.FieldTag' has every tag prefix.
tagsForest :: [(K.TagSegments, a)] -> Tree.Forest a
tagsForest [] = []
tagsForest ((segs, x) : rest) = Tree.Node x (tagsForest children) : tagsForest others
  where (children, others) = span ((segs `isPrefixOf`) . fst) rest


-- | Number of pages listed on the browse page without intersection
recentPagesCount :: Int
recentPagesCount = 20

-- | Browse page: the pages matching the request, links to toggle the keys of
-- union fields (sources), and links to narrow the request with intersection
-- fields (next tags, custom fields keys) or to remove one of their keys.
-- Without intersection in the request (none at all, or only sources), only
-- the most recently modified pages are listed. A link switches between the
-- archives and the other pages, when the user can read archives.
serveBrowseAll :: [(T.Text, T.Text)] -> ActM ()
serveBrowseAll params = do
  db <- getRestrictedDB
  ctx <- asksK K.context
  admin <- U.isAdmin
  archives <- hasArchives
  -- the sources and the archive mode are those of the context
  case U.decodeReq (K.customMetaConfig db) (U.withoutContextParams params) of
    Left err -> serveBadRequest err
    -- the access groups of the pages are only known by admins
    Right req | not admin && M.member K.FieldAccess req ->
                  serveBadRequest "unknown parameter: access"
    Right req ->
      let req' = U.sourcesReq (K.contextSources ctx) <> req
          switchCtx = ctx { K.contextArchive = not $ K.contextArchive ctx }
          switch = if archives || K.contextArchive ctx
                   then Just $ U.browseUrl switchCtx req' else Nothing
      in metaIndex db ctx req' switch >>= html
  where
    metaIndex db ctx req switch =
      let idx = K.pagesIndex db
          subset = if M.null req then idx
                   else DI.narrow idx $ U.reqToQuery req
          reqKeySet f = M.findWithDefault S.empty f req
          reqKeys = S.toList . reqKeySet
          isUnion f = case U.fieldCombination f of
            U.Union        -> True
            U.Intersection -> False
          -- keys of a field that can be in the request: those of the selected
          -- pages narrow it, while alternatives to an union are in all pages,
          -- with the selected ones, so that they can be removed (a source
          -- without pages in the archives)
          fieldKeys f = if isUnion f
                        then S.toList $ S.fromList (DI.getFieldKeys idx f) <> reqKeySet f
                        else DI.getFieldKeys subset f
          -- tags specific handling
          allTagsSegs = tagsListSubSegments $ K.fromKeys $ fieldKeys K.FieldTag
          queryTagsSegs = tagsListSubSegments $ K.fromKeys $ reqKeys K.FieldTag
          nextTags = filterRootTags $ fastListDiff allTagsSegs queryTagsSegs
          tags = (K.FieldTag, map (K.KeyText . U.segmentsToTag) nextTags)
          -- sources and custom fields
          fields = K.FieldSource : relevantFields subset (K.customMetaConfig db)
          -- union fields: each key is a toggle, adding or removing it
          toggles = [ (f, map (toggle f) $ fieldKeys f) | f <- fields, isUnion f ]
          toggle f k
            | S.member k (reqKeySet f) =
                V.Toggle (keyLabel f k) (U.browseUrl ctx $ U.removeFromReq req (f,k)) True
            | otherwise =
                V.Toggle (keyLabel f k) (U.browseUrl ctx $ U.addToReq req (f,k)) False
          -- intersection fields: the keys narrowing the request, and the keys
          -- of the request with a link to remove them
          menu = tags : filterOut [ (f, fieldKeys f) | f <- fields, not (isUnion f) ]
          nextStep = map (\(f, keys) -> (f, map (nextStepLink f) keys)) $ menu
          -- keys already in the request are not proposed again
          filterOut = mapMaybe $ \(f, ks) ->
            case filter (`S.notMember` reqKeySet f) ks of
              [] -> Nothing
              a  -> Just (f, a)
          active = [ (f, keyLabel f k, U.browseUrl ctx $ U.removeFromReq req (f,k))
                   | (f, ks) <- M.toList req, not (isUnion f), k <- S.toList ks ]
          -- produce a label and a link to a narrower request
          nextStepLink f k = (nextStepLabel f k, U.browseUrl ctx $ U.addToReq req (f,k))
          -- most recently modified first, only the latest ones without
          -- intersection
          byMTime = sortOn (Down . K.pageMTime) $ DI.documentsList subset
          listing = if all isUnion (M.keys req) then V.RecentPages else V.AllPages
          pages = case listing of
            V.RecentPages -> take recentPagesCount byMTime
            V.AllPages    -> byMTime
      in V.renderBrowseAll toggles switch nextStep active listing pages

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


-- | Full text search, in the pages of the navigation context. The query is
-- stemmed in the default lang, unless it starts with a lang code ("fr:")
serveSearch :: T.Text -> ActM ()
serveSearch query = do
  db <- getRestrictedDB
  ctx <- asksK K.context
  let sources = K.contextSources ctx
      lang = K.baseLang $ K.metaLang $ K.defaultMeta db
      -- in the restricted index, and in the sources context
      visible uid = isJust (findPage db uid) &&
                    (S.null sources || S.member (fst uid) sources)
      pages = mapMaybe (findPage db) $
              searchPages (K.searchEngine db) visible (queryTerms lang query)
    in case pages of
         -- un seul résultat, on y va
         (p:[]) -> redirect $ TL.fromStrict $ U.inContext ctx $ U.pageUrl p
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
      logMsg L.Warn $ "login failed: " <> name
      r <- V.renderLoginForm True
      status Net.status401
      html r
    Just (user, groups) -> do
      key <- liftIO $ U.randomText 128
      now <- liftIO $ Time.getCurrentTime
      U.modifySessions $ M.insert key (user, groups, now)
      U.setKiwiCookie key
      logMsg L.Info $ "login: " <> user
      redirect "/"


logUserOut :: ActM ()
logUserOut = do
  U.deleteKiwiCookie
  authM <- U.getKiwiCookie
  case authM of
    Nothing -> redirect "/"
    Just auth -> do
      sessions <- U.getSessions
      U.modifySessions $ M.delete auth
      forM_ (M.lookup auth sessions) $ \(user, _, _) -> logMsg L.Info $ "logout: " <> user
      redirect "/"


logMsg :: L.LogLevel -> T.Text -> ActM ()
logMsg level msg = do
  logger <- asksK K.logger
  liftIO $ logger level msg


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


-- | Set the navigation context from the parameters of the URL (see
-- 'U.contextParams'), so that each browser tab keeps its own. Unknown sources
-- are ignored, and "archive" can only be "yes".
withContext :: ActM () -> ActM ()
withContext run = do
  params <- queryParams
  db <- getDB
  let known = DI.getFieldKeys (K.pagesIndex db) K.FieldSource
      sources = S.fromList [ s | ("source", s) <- params, K.KeyText s `elem` known ]
  case [ v | ("archive", v) <- params ] of
    vs | all (== "yes") vs ->
           local (\ss -> ss { K.context = K.Context sources (not $ null vs) }) run
    _ -> serveBadRequest "invalid value for archive"


ifAdmin :: ActM () -> ActM ()
ifAdmin a = withLogin $ do
  adm <- U.isAdmin
  if adm then a
    else serveForbidden


-- | The pages the user can read, archived or not according to the context
getRestrictedDB :: ActM K.PagesDB
getRestrictedDB = do
  db <- getDB
  query <- readableQuery =<< asksK (K.contextArchive . K.context)
  return db { K.pagesIndex = DI.narrow (K.pagesIndex db) query }

-- | Whether the user can read archived pages
hasArchives :: ActM Bool
hasArchives = do
  db <- getDB
  query <- readableQuery True
  return $ not $ S.null $ DI.queryIdSet (K.pagesIndex db) query

-- | The pages the user can read, archived or not: "public" ones for anonymous
-- users, those of their groups too once logged in, all of them for admins
readableQuery :: Bool -> ActM (DI.Query K.MetaField K.DocIndexKey)
readableQuery archive = do
  loginM <- asksK K.login
  isAdmin <- U.isAdmin
  let world = U.qOnly K.FieldArchive archive
      readable groups = DI.And [U.qAny K.FieldAccess ("public" : groups :: [T.Text]), world]
  return $ case loginM of
    Nothing -> readable []
    Just (_, g, _) | isAdmin   -> world
                   | otherwise -> readable g

