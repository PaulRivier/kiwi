module Kiwi.Server
    ( kiwiServer
    , securityHeaders  -- exported for tests
    , initServerState  -- exported for tests
    , kiwiApp          -- exported for tests
    , reloadKiwi       -- exported for tests
    ) where


import           Control.Concurrent.QSem (newQSem)
import           Control.Concurrent.MVar (newMVar, withMVar)
import           Control.Exception (IOException, SomeException, try)
import           Control.Monad (when)
import           Control.Monad.Reader
import           Data.IORef (newIORef, readIORef)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Network.HTTP.Types as Net
import qualified Network.Wai as Wai
import           Data.String (fromString)
import           Data.Version (showVersion)
import           Network.Wai.Handler.Warp (setBeforeMainLoop, setHost, setPort)
import           Network.Wai.Middleware.AddHeaders (addHeaders)
import           Network.Wai.Middleware.Gzip (gzip, defaultGzipSettings)
import           Network.Wai.Middleware.RequestLogger (DetailedSettings (..), OutputFormat (..),
                                                       RequestLoggerSettings (..),
                                                       defaultDetailedSettings,
                                                       defaultRequestLoggerSettings,
                                                       mkRequestLogger)
import qualified System.Directory as D
import           System.Exit (exitFailure)
import qualified System.FilePath as FP
import           Text.Printf (printf)
import           Web.Scotty.Trans

import           Kiwi.Auth (loadAccounts, loadSessions, refusedLinesWarning, reloadAccountsIO)
import qualified Kiwi.ConfigFile as Conf
import           Kiwi.Api
import           Kiwi.Controller
import qualified Kiwi.Log as L
import qualified Kiwi.PagesDB as DB
import           Kiwi.Theme (serveTheme)
import           Kiwi.Types
import           Kiwi.Utils (timeIO)
import           Paths_kiwi (version)


-- | Serves a kiwi. Once its pages are loaded, the action that reloads it
-- (see 'reloadKiwi') is given to the last argument, for the reload signal.
-- Each step is logged once done: start, pages loaded, listening (and the
-- keywords, see 'DB.updatePagesDB').
kiwiServer :: FP.FilePath -> Conf.KiwiConfig -> L.LogLevel -> (IO () -> IO ()) -> IO ()
kiwiServer cfp c logLevel onReload = do
  let logger' = L.mkLogger logLevel (Conf.name c)
  logger' L.Info $ T.pack $ "Kiwi " ++ showVersion version ++ " starting"
  (ss, t) <- timeIO $ initServerState cfp c logger'
  onReload $ reloadKiwi ss
  nb <- DB.pagesCount <$> readIORef (pagesDB ss)
  logger' L.Info $ T.pack $ concat [ nb, " pages loaded in ", printf "%.2f" t, "s" ]
  -- HTTP requests are only logged in debug mode
  requestLogger <- mkRequestLogger defaultRequestLoggerSettings
    { outputFormat = DetailedWithSettings defaultDetailedSettings
        { useColors = False
        , mModifyParams = Just hidePassword } }
  let runActionToIO m = runReaderT (runServerM m) ss
      -- no scotty startup banner
      opts = defaultOptions { verbose = 0
                            , settings = maybe id (setHost . fromString) (Conf.bind c) $
                                         setPort (Conf.port c) $
                                         setBeforeMainLoop listening (settings defaultOptions) }
      -- once the socket is open
      listening = logger' L.Info $ T.pack $ concat
        [ "listening on ", maybe "port " (++ ":") (Conf.bind c), show (Conf.port c) ]
  api <- apiApp ss
  scottyOptsT opts runActionToIO $ do
    when (logLevel == L.Debug) $ middleware requestLogger
    kiwiRoute ss api
  where
    -- the login password is never logged (nor the raw request body, which
    -- is not displayed when params are modified)
    hidePassword ("password", _) = Just ("password", "***")
    hidePassword p = Just p


-- | The application of a kiwi, as served by 'kiwiServer' (without the
-- requests logger of the debug mode)
kiwiApp :: ServerState -> IO Wai.Application
kiwiApp ss = do
  api <- apiApp ss
  scottyAppT defaultOptions { verbose = 0 }
             (\m -> runReaderT (runServerM m) ss)
             (kiwiRoute ss api)


-- | The routes of a kiwi, given the application of its API (see 'apiApp')
kiwiRoute :: ServerState -> Wai.Application -> WebM ()
kiwiRoute ss api = do
  -- the first middleware is the outer one: all the responses have the headers
  middleware securityHeaders
  -- compressed responses, for the clients that accept them: text types, and
  -- not the files of the sources
  middleware $ gzip defaultGzipSettings
  -- exceptions of the actions (redirections are not exceptions for it)
  defaultHandler $ Handler $ withLogin . serveError
  get "/" $ redirect "/browse"
  get "/page/:source/:pId" $ do
    source <- pathParam "source"
    pId <- pathParam "pId"
    withLogin $ withContext $ withETag $ servePage (source, pId)
  get "/browse" $ withLogin $ withContext $ withETag $ queryParams >>= serveBrowseAll
  get "/search" $ withLogin $ withContext $ withETag $ do
    -- without query, the search form is displayed
    query <- queryParamMaybe "query"
    serveSearch $ fromMaybe "" query
  -- in the archive mode, but not in the sources context
  get "/metadata" $ withLogin $ withContext $ withETag serveMetadata
  get "/metadata-editor" $ withLogin $ withContext serveMetadataEditor
  post "/metadata-editor" $ withLogin $ withContext serveMetaScript
  get "/doctor" $ withLogin $ withContext $ withETag serveDoctor
  -- documentation, out of the navigation context
  get "/readme" $ withLogin serveReadme
  get "/commonmark" $ withLogin serveCommonMark
  post "/reload" $ ifAdmin $ do
    liftIO $ reloadKiwi ss
    html ""
  -- without editor, the edition is disabled: the route does not exist
  post "/edit-page" $ case editorCommand ss of
    Nothing -> serveNotFound
    Just ed -> ifAdmin $ do
      source <- formParam "page-source"
      pId <- formParam "page-id"
      serveEditPage ed (source, pId)
  -- edition of the pages in the browser, with the write right
  get "/edit/:source/:pId" $ withLogin $ do
    source <- pathParam "source"
    pId <- pathParam "pId"
    line <- queryParamMaybe "line"
    serveEditor (source, pId) line
  post "/save-page" $ withLogin $ do
    (uid, sent) <- editParams
    hash <- formParam "hash"
    serveSavePage uid hash sent
  post "/preview" $ withLogin $ editParams >>= uncurry servePreview
  get  "/login" $ withLogin serveLogin
  post "/login" $ logUserIn
  post "/logout" $ logUserOut

  -- the API, with a token (see "Kiwi.Api"), and the MCP of the agents
  apiRoute
  post "/mcp" $ withToken (serveMcp api)
  get "/mcp" mcpMethodNotAllowed
  delete "/mcp" mcpMethodNotAllowed

  middleware serveTheme

  -- the files of the sources linked by the pages, by uid (see 'serveFile')
  get "/file/:source/:uid" $ do
    source <- captureParam "source"
    uid <- captureParam "uid"
    withLogin $ serveFile source uid

  notFound $ withLogin serveNotFound

  where
    editParams = do
      source <- formParam "source"
      pId <- formParam "id"
      -- browsers send the text of a textarea with CRLF line endings
      sent <- T.replace "\r\n" "\n" <$> formParam "text"
      return ((source, pId), sent)


-- | The routes of the API (see "Kiwi.Api")
apiRoute :: WebM ()
apiRoute = do
  get "/api/pages" $ withToken apiPages
  get "/api/facets" $ withToken apiFacets
  get "/api/page/:source/:uid" $ do
    source <- pathParam "source"
    uid <- pathParam "uid"
    withToken $ apiPage source uid
  get "/api/page/:source/:uid/meta" $ do
    source <- pathParam "source"
    uid <- pathParam "uid"
    withToken $ apiPageMeta source uid
  get "/api/guide" $ withToken apiGuide
  get "/api/search" $ withToken apiSearch
  put "/api/page/:source/:uid" $ do
    source <- pathParam "source"
    uid <- pathParam "uid"
    withToken $ apiPutPage source uid
  patch "/api/page/:source/:uid" $ do
    source <- pathParam "source"
    uid <- pathParam "uid"
    withToken $ apiPatchPage source uid
  post "/api/pages/:source" $ do
    source <- pathParam "source"
    withToken $ apiCreatePage source
  post "/api/metadata" $ withToken apiMetadata

-- | The API alone, as an application, for the tools of the MCP (see
-- 'serveMcp')
apiApp :: ServerState -> IO Wai.Application
apiApp ss = scottyAppT defaultOptions { verbose = 0 }
                       (\m -> runReaderT (runServerM m) ss)
                       (defaultHandler (Handler $ withLogin . serveError) >> apiRoute)

-- | GET and DELETE /mcp: no stream from the server, no session
mcpMethodNotAllowed :: ActM ()
mcpMethodNotAllowed = do
  status Net.status405
  setHeader "Allow" "POST"


-- | Security headers of all the responses (pages, theme, images and files of
-- the sources, errors): only the scripts of kiwi run (not those of the raw
-- HTML of the pages, nor those of the HTML files of the sources), no plugin,
-- no other base URL, no framing by other sites, no MIME type sniffing
securityHeaders :: Wai.Middleware
securityHeaders = addHeaders
  [ ( "Content-Security-Policy"
    , "script-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'self'" )
  , ("X-Content-Type-Options", "nosniff") ]


-- | Reads the accounts file again, and reloads the pages: for the reload
-- button of the admins, and for the reload signal (kiwi reload). A failure
-- is logged, and leaves the kiwi as it was.
reloadKiwi :: ServerState -> IO ()
reloadKiwi ss = do
  r <- try $ reloadAccountsIO ss >> updateDB ss
  case r of
    Right () -> return ()
    Left e -> logger ss L.Error $ T.pack $ "reload failed: " ++ show (e :: SomeException)


-- | Reloads are serialized by pagesDBLock, so that two concurrent reloads
-- can not overwrite each other. Readers are never blocked.
updateDB :: ServerState -> IO ()
updateDB ss = do
  withMVar (pagesDBLock ss) $ \_ -> DB.reloadPages ss


initServerState :: FP.FilePath -> Conf.KiwiConfig -> L.Logger -> IO ServerState
initServerState cfp conf logger' = do
  cfp' <- D.makeAbsolute cfp
  let kiwiDir' = FP.takeDirectory cfp'
  let contentDir' = FP.combine kiwiDir' (Conf.contentDir conf)
  let accountsFile = FP.combine kiwiDir' "_accounts"
  accountsE <- try $ loadAccounts accountsFile
  accounts' <- case accountsE of
    Right (accs, refused) -> do
      mapM_ (logger' L.Warn . T.pack) $ refusedLinesWarning accountsFile refused
      return accs
    Left e -> do
      logger' L.Error $ T.pack $ concat
        [ "unable to read the accounts file ", accountsFile, ": ", show (e :: IOException) ]
      exitFailure
  accR <- newIORef accounts'
  accLock <- newMVar ()
  sem <- newQSem 3
  sess      <- loadSessions $ FP.combine kiwiDir' "_sessions"
  sessM <- newMVar sess
  db <- DB.updatePagesDB logger' (Conf.gitCommit conf) contentDir'
          (DB.emptyPagesDB (Conf.defaultMeta conf)
                           (Conf.customMetaConfig conf)
                           (Conf.sourcesConfig conf))
  DB.warmUpSearchEngine (searchEngine db)
  dbR <- newIORef db
  dbLock <- newMVar ()
  etags' <- newMVar M.empty
  return $ ServerState {
      kiwiName   = Conf.name conf
    , contentDir = contentDir'
    , kiwiDir    = kiwiDir'
    , editorCommand = Conf.editor conf
    , uiLang       = findInterfaceLang (Conf.uiLang conf)
    , kiwiPort     = Conf.port conf
    , accounts     = accR
    , accountsLock = accLock
    , loginSem     = sem
    , sessions     = sessM
    , requestState = RequestState Nothing noContext
    , pagesDB      = dbR
    , pagesDBLock  = dbLock
    , etags        = etags'
    , tocSetting   = Conf.toc conf
    , secureCookie = Conf.secureCookie conf
    , gitCommit    = Conf.gitCommit conf
    , agentGuide   = Conf.agentGuideFile cfp' conf
    , logger       = logger'
  }
  where
    findInterfaceLang :: UI_Lang -> UI_Lang
    findInterfaceLang UI_French = UI_French
    findInterfaceLang _         = UI_English


-- The files of the sources were served from their images and files folders,
-- without access control; they are now served by 'serveFile'.
-- serveStatic :: Static.CacheContainer -> [(String, FP.FilePath)] -> WebM ()
-- serveStatic cache parts =
--   let opts = Static.defaultOptions { Static.cacheContainer = cache }
--       basePolicy = mconcat [ Static.noDots
--                            , Static.isNotAbsolute
--                            , Static.predicate (not . isInfixOf "/.")
--                            ]
--       contentPolicies = map (\(url,path) -> contentPolicy url path) parts
--       policy = basePolicy >-> foldl1 (<|>) contentPolicies
--   in middleware $
--      Static.staticPolicyWithOptions opts policy
--   where
--     contentPolicy url fspath = mconcat [ Static.policy (stripPrefix url)
--                                        , Static.addBase fspath ]


