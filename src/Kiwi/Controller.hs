module Kiwi.Controller
  ( servePage
  , serveBrowseAll
  , serveSearch
  , serveMetadata
  , serveDoctor
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
  , BrowseAll (..)    -- exported for tests
  , metaIndex         -- exported for tests
  ) where

import           Control.Concurrent (forkIO)
import           Control.Exception (IOException, try)
import           Control.Monad (forM_)
import           Control.Monad.Reader
import qualified Data.ByteString as BS
import           Data.IORef (readIORef)
import           Data.Function (on)
import           Data.List (groupBy, isPrefixOf, sortOn)
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
import           System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import qualified System.Process as Proc
import qualified Text.Pandoc.Definition as PD
import qualified Network.Wai as Wai
import           Web.Scotty.Trans (formParam, queryParams, redirect, request, status, html)

import           Kiwi.Types (ActM)
import qualified Kiwi.Log as L
import qualified Kiwi.DefaultPages as DP
import           Kiwi.Pandoc (pageDoc, embeddedDoc)
import           Kiwi.Search (searchPages, queryTerms, queryLang, searchLang, normalizeText)
import           Kiwi.PagesDB (brokenLinks)
import qualified Kiwi.Types as K
import qualified Kiwi.View as V
import qualified Kiwi.Auth as Auth
import qualified Kiwi.Browse as Browse
import qualified Kiwi.Tags as Tags
import qualified Kiwi.Url as Url
import           Kiwi.Utils (getDB, asksK, fastListDiff)
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
          -- links to the pages the user can not read are shown as broken
          pageExists = isJust . findPage db
      r <- V.renderPage pageExists p doc backLinkPages
      html r
    Nothing -> serveNotFound

-- | The document, or a document telling that it can not be rendered
orRenderError :: FP.FilePath -> Either String PD.Pandoc -> ActM PD.Pandoc
orRenderError _ (Right doc) = return doc
orRenderError path (Left e) = do
  logMsg L.Warn $ T.pack $ concat ["unable to render ", path, " : ", e]
  return $ PD.Pandoc mempty [PD.Para [PD.Str "Unable to render this page."]]


-- | Issues of the content, among the pages the user can read, archived or not
-- according to the context (not in the sources context): the links to pages
-- that the user can not read (missing, or not readable), as displayed with
-- the class kiwi-link-broken.
serveDoctor :: ActM ()
serveDoctor = do
  db <- getRestrictedDB
  -- brokenLinks gives the links page by page, in the order of the file
  -- paths, so source by source
  let byPage = [ (pg, map (snd . snd) links)
               | links@((pg, _) : _) <- groupBy ((==) `on` (K.pageUID . fst))
                                                (brokenLinks $ K.pagesIndex db) ]
      bySource = [ (fst $ K.pageUID pg, pages)
                 | pages@((pg, _) : _) <- groupBy ((==) `on` (fst . K.pageUID . fst)) byPage ]
  r <- V.renderDoctor $ sortOn (textOrder . fst) bySource
  html r


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
  admin <- Auth.isAdmin
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
        , V.metaLink  = Browse.browseUrl ctx $ Browse.addToReq (Browse.sourcesReq $ S.singleton s) (f, k)
        , V.metaCount = S.size $ DI.lookupIndex sub f k }
  V.renderMetadata (map section sources) >>= html
  where
    -- a sub-tag shows its last segment, below its parent
    keyLabel K.FieldTag (K.KeyText t) = case reverse (Tags.tagToSegments t) of
      seg : _ -> seg
      []      -> t
    keyLabel _ k = Browse.showKeyLabel k
    keyCopy K.FieldTag (K.KeyText t) = Tags.prettyTag t
    keyCopy _ k = Browse.showKeyLabel k
    -- texts without case nor accents, tags segment by segment (the raw
    -- segment breaks ties, so that a tag is followed by its sub-tags), the
    -- other keys (numbers, dates...) in their natural order
    keysForest K.FieldTag keys = tagsForest $ sortOn (map segOrder . fst)
      [ (Tags.tagToSegments t, k) | k@(K.KeyText t) <- keys ]
    keysForest _ keys = [ Tree.Node k [] | k <- sortOn keyOrder keys ]
    segOrder seg = (textOrder seg, seg)
    keyOrder (K.KeyText t) = Left (textOrder t, t)
    keyOrder k = Right k

-- | Order of the texts (sources, metadata keys), without case nor accents
textOrder :: T.Text -> T.Text
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
  admin <- Auth.isAdmin
  archives <- hasArchives
  -- the sources and the archive mode are those of the context
  case Browse.decodeReq (K.customMetaConfig db) (Url.withoutContextParams params) of
    Left err -> serveBadRequest err
    -- the access groups of the pages are only known by admins
    Right req | not admin && M.member K.FieldAccess req ->
                  serveBadRequest "unknown parameter: access"
    Right req ->
      let req' = Browse.sourcesReq (K.contextSources ctx) <> req
          switchCtx = ctx { K.contextArchive = not $ K.contextArchive ctx }
          switch = if archives || K.contextArchive ctx
                   then Just $ Browse.browseUrl switchCtx req' else Nothing
          ba = metaIndex db ctx req'
      in V.renderBrowseAll (baToggles ba) switch (baNextStep ba) (baActive ba)
                           (baListing ba) (baPages ba) >>= html

-- | Contents of the browse page (see 'serveBrowseAll'), except the link to
-- switch between the archives and the other pages
data BrowseAll = BrowseAll
  { baToggles  :: [(K.MetaField, [V.Toggle])]          -- ^ keys of union fields
  , baNextStep :: [(K.MetaField, [(T.Text, T.Text)])]  -- ^ (label, link) narrowing the request
  , baActive   :: [(K.MetaField, T.Text, T.Text)]      -- ^ (field, label, link removing it)
  , baListing  :: V.PagesListing
  , baPages    :: [K.Page]
  }

-- | Contents of the browse page of a request, from the pages the user can
-- read, with links in the navigation context
metaIndex :: K.PagesDB -> K.Context -> Browse.BrowseAllReq -> BrowseAll
metaIndex db ctx req =
  let idx = K.pagesIndex db
      subset = if M.null req then idx
               else DI.narrow idx $ Browse.reqToQuery req
      reqKeySet f = M.findWithDefault S.empty f req
      reqKeys = S.toList . reqKeySet
      isUnion f = case Browse.fieldCombination f of
        Browse.Union        -> True
        Browse.Intersection -> False
      -- keys of a field that can be in the request: those of the selected
      -- pages narrow it, while alternatives to an union are those of the
      -- pages matching the other fields of the request, with the selected
      -- ones, so that they can be removed (a source without pages with the
      -- tags of the request, or in the archives)
      fieldKeys K.FieldSource = S.toList $ otherSources <> reqKeySet K.FieldSource
      fieldKeys f = if isUnion f
                    then S.toList $ S.fromList (DI.getFieldKeys idx f) <> reqKeySet f
                    else DI.getFieldKeys subset f
      -- sources of the pages matching the other fields of the request,
      -- without narrowing the index again: from the ids (source, page id)
      otherSources
        | M.null others = S.fromList $ DI.getFieldKeys idx K.FieldSource
        | not (M.member K.FieldSource req) =
            S.fromList $ DI.getFieldKeys subset K.FieldSource
        | otherwise = S.map (K.KeyText . fst) $ DI.queryIdSet idx $
                      Browse.reqToQuery others
        where others = M.delete K.FieldSource req
      -- tags specific handling
      allTagsSegs = Tags.tagsListSubSegments $ K.fromKeys $ fieldKeys K.FieldTag
      queryTagsSegs = Tags.tagsListSubSegments $ K.fromKeys $ reqKeys K.FieldTag
      nextTags = Tags.filterRootTags $ fastListDiff allTagsSegs queryTagsSegs
      tags = (K.FieldTag, map (K.KeyText . Tags.segmentsToTag) nextTags)
      -- sources and custom fields
      fields = K.FieldSource : relevantFields subset (K.customMetaConfig db)
      -- union fields: each key is a toggle, adding or removing it
      toggles = [ (f, map (toggle f) $ fieldKeys f) | f <- fields, isUnion f ]
      toggle f k
        | S.member k (reqKeySet f) =
            V.Toggle (keyLabel f k) (Browse.browseUrl ctx $ Browse.removeFromReq req (f,k)) True
        | otherwise =
            V.Toggle (keyLabel f k) (Browse.browseUrl ctx $ Browse.addToReq req (f,k)) False
      -- intersection fields: the keys narrowing the request, and the keys
      -- of the request with a link to remove them
      menu = tags : filterOut [ (f, fieldKeys f) | f <- fields, not (isUnion f) ]
      nextStep = map (\(f, keys) -> (f, map (nextStepLink f) keys)) $ menu
      -- keys already in the request are not proposed again
      filterOut = mapMaybe $ \(f, ks) ->
        case filter (`S.notMember` reqKeySet f) ks of
          [] -> Nothing
          a  -> Just (f, a)
      active = [ (f, keyLabel f k, Browse.browseUrl ctx $ Browse.removeFromReq req (f,k))
               | (f, ks) <- M.toList req, not (isUnion f), k <- S.toList ks ]
      -- produce a label and a link to a narrower request
      nextStepLink f k = (nextStepLabel f k, Browse.browseUrl ctx $ Browse.addToReq req (f,k))
      -- most recently modified first, only the latest ones without
      -- intersection
      byMTime = sortOn (Down . K.pageMTime) $ DI.documentsList subset
      listing = if all isUnion (M.keys req) then V.RecentPages else V.AllPages
      pages = case listing of
        V.RecentPages -> take recentPagesCount byMTime
        V.AllPages    -> byMTime
  in BrowseAll toggles nextStep active listing pages
  where
    nextStepLabel K.FieldTag (K.KeyText tag) = case Tags.tagToSegments tag of
      (t:[]) -> t
      segs      -> T.concat ["> ", last segs]
    nextStepLabel f k = keyLabel f k

    keyLabel K.FieldTag (K.KeyText tag) = Tags.prettyTag tag
    keyLabel _ k = Browse.showKeyLabel k

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
-- stemmed in the lang of the sources of the context (see 'searchLang'), unless
-- it starts with a lang code ("fr:")
serveSearch :: T.Text -> ActM ()
serveSearch query = do
  db <- getRestrictedDB
  allDB <- getDB
  ctx <- asksK K.context
  let sources = K.contextSources ctx
      -- all the sources, as those the context can have (see 'withContext')
      allSources = [ s | K.KeyText s <- DI.getFieldKeys (K.pagesIndex allDB) K.FieldSource ]
      ctxLang = searchLang (K.metaLang $ K.defaultMeta allDB) (K.sourcesConfig allDB)
                           allSources sources
      -- the lang prefix of the query, if any, is displayed with the query
      lang = fst $ queryLang ctxLang query
      -- in the restricted index, and in the sources context
      visible uid = isJust (findPage db uid) &&
                    (S.null sources || S.member (fst uid) sources)
      pages = mapMaybe (findPage db) $
              searchPages (K.searchEngine db) visible (queryTerms ctxLang query)
    in case pages of
         -- un seul résultat, on y va
         (p:[]) -> redirect $ TL.fromStrict $ Url.inContext ctx $ Url.pageUrl p
         _      -> do r <- V.renderSearchResults lang query pages
                      html r


-- | Runs the editor of the config (command and arguments) on the file of the
-- page. Without editor, the route answers 404 (see 'Kiwi.Server').
serveEditPage :: (FP.FilePath, [String]) -> K.PageUID -> ActM ()
serveEditPage (cmd, args) pgId = do
  db <- getDB
  case findPage db pgId of
    Nothing -> serveNotFound
    Just p -> do
      logger <- asksK K.logger
      let editorFailed msg = logger L.Warn $ T.pack $ "editor failed: " ++ msg
      -- the editor may outlive the request: wait for it in a thread,
      -- so that it does not stay a zombie, and log its failures
      _ <- liftIO $ forkIO $ do
        r <- try $ Proc.spawnProcess cmd (args ++ [K.pageAbsoluteFSPath p]) >>= Proc.waitForProcess
        case r of
          Left e -> editorFailed $ show (e :: IOException)
          Right (ExitFailure c) -> editorFailed $ cmd ++ " : exit code " ++ show c
          Right ExitSuccess -> pure ()
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
  addr <- Auth.clientAddress . show . Wai.remoteHost <$> request
  -- the accounts file is read again: a new account can log in at once
  Auth.reloadAccounts
  accounts <- liftIO . readIORef =<< asksK K.accounts
  sem <- asksK K.loginSem
  accountM <- liftIO $ Auth.checkLogin sem accounts name pass
  case accountM of
    Nothing -> do
      logMsg L.Warn $ "login failed from " <> addr <> ": " <> Auth.sanitizeLogText name
      r <- V.renderLoginForm True
      status Net.status401
      html r
    Just account -> do
      let user = K.accountId account
      token <- liftIO Auth.newSessionToken
      now <- liftIO $ Time.getCurrentTime
      Auth.modifySessions $ M.insert (Auth.hashToken token) (Auth.newSession account now)
      Auth.setKiwiCookie token
      logMsg L.Info $ "login from " <> addr <> ": " <> user
      redirect "/"


logUserOut :: ActM ()
logUserOut = do
  Auth.deleteKiwiCookie
  authM <- Auth.getKiwiCookie
  case authM of
    Nothing -> redirect "/"
    Just token -> do
      let key = Auth.hashToken token
      sessions <- Auth.getSessions
      Auth.modifySessions $ M.delete key
      forM_ (M.lookup key sessions) $ \s -> logMsg L.Info $ "logout: " <> K.sessionUser s
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


-- | 403 for a logged in user without the right, 401 for an anonymous one,
-- who may have it once logged in
serveForbidden :: ActM ()
serveForbidden = do
  loginM <- asksK K.login
  r <- V.renderForbidden
  status $ if isJust loginM then Net.status403 else Net.status401
  html r


findPage :: K.PagesDB -> K.PageUID -> Maybe K.Page
findPage db pgId = DI.findDoc (K.pagesIndex db) pgId 


-- | Set login from session cookie, if the session exists, has not expired,
-- and its account still exists (with the current groups of the account)
withLogin :: ActM a -> ActM a
withLogin run = do
  login' <- Auth.requestLogin
  local (setLogin login') run
  where
    setLogin l ss = ss { K.login = l }


-- | Set the navigation context from the parameters of the URL (see
-- 'Url.contextParams'), so that each browser tab keeps its own. Unknown sources
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
  adm <- Auth.isAdmin
  if adm then a
    else serveForbidden


-- | The pages the user can read, archived or not according to the context
getRestrictedDB :: ActM K.PagesDB
getRestrictedDB = do
  db <- getDB
  loginM <- asksK K.login
  archive <- asksK (K.contextArchive . K.context)
  return db { K.pagesIndex = DI.narrow (K.pagesIndex db) (Auth.readableQuery loginM archive) }

-- | Whether the user can read archived pages
hasArchives :: ActM Bool
hasArchives = do
  db <- getDB
  loginM <- asksK K.login
  return $ not $ S.null $ DI.queryIdSet (K.pagesIndex db) (Auth.readableQuery loginM True)

