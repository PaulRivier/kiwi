module Kiwi.Server
    ( kiwiServer
    , securityHeaders  -- exported for tests
    , initServerState  -- exported for tests
    , kiwiApp          -- exported for tests
    ) where


import           Control.Concurrent (forkIO)
import           Control.Concurrent.QSem (newQSem)
import           Control.Concurrent.MVar (newMVar, withMVar)
import           Control.Exception (IOException, evaluate, try)
import           Control.Monad (when, forM_, void)
import           Control.Monad.Reader
import           Data.IORef (newIORef, readIORef, atomicWriteIORef)
import           Data.List (stripPrefix, isInfixOf)
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.SearchEngine as SE
import qualified Data.Text as T
import qualified Network.Wai as Wai
import           Network.Wai.Handler.Warp (setPort)
import           Network.Wai.Middleware.AddHeaders (addHeaders)
import           Network.Wai.Middleware.Gzip (gzip, defaultGzipSettings)
import           Network.Wai.Middleware.RequestLogger (DetailedSettings (..), OutputFormat (..),
                                                       RequestLoggerSettings (..),
                                                       defaultDetailedSettings,
                                                       defaultRequestLoggerSettings,
                                                       mkRequestLogger)
import qualified Network.Wai.Middleware.Static as Static
import           Network.Wai.Middleware.Static ((>->), (<|>))
import qualified System.Directory as D
import           System.Exit (exitFailure)
import qualified System.FilePath as FP
import           Text.Printf (printf)
import           Web.Scotty.Trans

import           Kiwi.Auth (loadAccounts, loadSessions, reloadAccounts)
import qualified Kiwi.ConfigFile as Conf
import           Kiwi.Controller
import qualified Kiwi.Log as L
import qualified Kiwi.PagesDB as DB
import           Kiwi.Theme (serveTheme)
import           Kiwi.Types
import           Kiwi.Utils (asksK, isDotFile, timeIO)
import qualified Utils.DocIndex as DI


pagesFSDir, imagesFSDir, filesFSDir :: FilePath
pagesFSDir = "pages"
filesFSDir = "files"
imagesFSDir = "images"


kiwiServer :: FP.FilePath -> Conf.KiwiConfig -> L.LogLevel ->
              Static.CacheContainer -> IO ()
kiwiServer cfp c logLevel cache = do
  let logger' = L.mkLogger logLevel (Conf.name c)
  (ss, t) <- timeIO $ initServerState cfp c logger'
  nb <- pagesCount <$> readIORef (pagesDB ss)
  logger' L.Info $ T.pack $ concat [ nb, " pages loaded in ", printf "%.2f" t
                                   , "s, listening on port ", show (Conf.port c) ]
  -- HTTP requests are only logged in debug mode
  requestLogger <- mkRequestLogger defaultRequestLoggerSettings
    { outputFormat = DetailedWithSettings defaultDetailedSettings
        { useColors = False
        , mModifyParams = Just hidePassword } }
  let runActionToIO m = runReaderT (runServerM m) ss
      -- no scotty startup banner
      opts = defaultOptions { verbose = 0
                            , settings = setPort (Conf.port c) (settings defaultOptions) }
  scottyOptsT opts runActionToIO $ do
    when (logLevel == L.Debug) $ middleware requestLogger
    kiwiRoute ss cache
  where
    -- the login password is never logged (nor the raw request body, which
    -- is not displayed when params are modified)
    hidePassword ("password", _) = Just ("password", "***")
    hidePassword p = Just p


-- | The application of a kiwi, as served by 'kiwiServer' (without the
-- requests logger of the debug mode)
kiwiApp :: ServerState -> Static.CacheContainer -> IO Wai.Application
kiwiApp ss cache = scottyAppT defaultOptions { verbose = 0 }
                              (\m -> runReaderT (runServerM m) ss)
                              (kiwiRoute ss cache)


kiwiRoute :: ServerState -> Static.CacheContainer -> WebM ()
kiwiRoute ss cache = do
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
  get "/doctor" $ withLogin $ withContext $ withETag serveDoctor
  -- documentation, out of the navigation context
  get "/readme" $ withLogin serveReadme
  get "/commonmark" $ withLogin serveCommonMark
  post "/reload" $ ifAdmin $ do
    reloadAccounts
    updateDB
    html ""
  -- without editor, the edition is disabled: the route does not exist
  post "/edit-page" $ case editorCommand ss of
    Nothing -> serveNotFound
    Just ed -> ifAdmin $ do
      source <- formParam "page-source"
      pId <- formParam "page-id"
      serveEditPage ed (source, pId)
  get  "/login" $ withLogin serveLogin
  post "/login" $ logUserIn
  post "/logout" $ logUserOut

  middleware serveTheme

  forM_ (contentSources ss) $ \src ->
    serveStatic cache $ filesParts (contentDir ss) src

  notFound $ withLogin serveNotFound

  where
    filesParts cd src = [ ( concat ["image/", src, "/"]
                          , FP.joinPath [cd, src, imagesFSDir] )
                        , ( concat ["file/", src, "/"]
                          , FP.joinPath [cd, src, filesFSDir]) ]
    


-- | Security headers of all the responses (pages, theme, images and files of
-- the sources, errors): only the scripts of kiwi run (not those of the raw
-- HTML of the pages, nor those of the HTML files of the sources), no plugin,
-- no other base URL, no framing by other sites, no MIME type sniffing
securityHeaders :: Wai.Middleware
securityHeaders = addHeaders
  [ ( "Content-Security-Policy"
    , "script-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'self'" )
  , ("X-Content-Type-Options", "nosniff") ]


-- | Reloads are serialized by pagesDBLock, so that two concurrent reloads
-- can not overwrite each other. Readers are never blocked.
updateDB :: ActM ()
updateDB = do
  dbR <- asksK pagesDB
  lock <- asksK pagesDBLock
  cd <- asksK contentDir
  logger' <- asksK logger
  liftIO $ withMVar lock $ \_ -> do
    db' <- readIORef dbR
    (db, t) <- timeIO $ DB.updatePagesDB logger' cd pagesFSDir db'
    atomicWriteIORef dbR db
    warmUpSearchEngine (searchEngine db)
    logger' L.Info $ T.pack $ concat [ "pages reloaded in ", printf "%.2f" t
                                     , "s, ", pagesCount db, " pages" ]


pagesCount :: PagesDB -> String
pagesCount = show . M.size . DI.store . pagesIndex


-- | Evaluate the search engine in a thread, so that the first search after
-- a load does not wait for it
warmUpSearchEngine :: PageSearchEngine -> IO ()
warmUpSearchEngine se = void $ forkIO $ void $ evaluate $ SE.invariant se


initServerState :: FP.FilePath -> Conf.KiwiConfig -> L.Logger -> IO ServerState
initServerState cfp conf logger' = do
  kiwiDir' <- D.makeAbsolute (FP.takeDirectory cfp)
  let contentDir' = FP.combine kiwiDir' (Conf.contentDir conf)
  sources <- filter (not . isDotFile) <$> D.listDirectory contentDir'
  let accountsFile = FP.combine kiwiDir' "_accounts"
  accountsE <- try $ loadAccounts accountsFile
  accounts' <- case accountsE of
    Right accs -> return accs
    Left e -> do
      logger' L.Error $ T.pack $ concat
        [ "unable to read the accounts file ", accountsFile, ": ", show (e :: IOException) ]
      exitFailure
  accR <- newIORef accounts'
  accLock <- newMVar ()
  sem <- newQSem 3
  sess      <- loadSessions $ FP.combine kiwiDir' "_sessions"
  sessM <- newMVar sess
  db <- DB.updatePagesDB logger' contentDir' pagesFSDir
          (DB.emptyPagesDB (Conf.defaultMeta conf)
                           (Conf.customMetaConfig conf)
                           (Conf.sourcesConfig conf))
  warmUpSearchEngine (searchEngine db)
  dbR <- newIORef db
  dbLock <- newMVar ()
  etags' <- newMVar M.empty
  return $ ServerState {
      kiwiName   = Conf.name conf
    , contentDir = contentDir'
    , contentSources = sources
    , kiwiDir    = kiwiDir'
    , editorCommand = Conf.editor conf
    , uiLang       = findInterfaceLang (Conf.uiLang conf)
    , kiwiPort     = Conf.port conf
    , accounts     = accR
    , accountsLock = accLock
    , loginSem     = sem
    , sessions     = sessM
    , login        = Nothing
    , context      = noContext
    , pagesDB      = dbR
    , pagesDBLock  = dbLock
    , etags        = etags'
    , tocSetting   = Conf.toc conf
    , secureCookie = Conf.secureCookie conf
    , logger       = logger'
  }
  where
    findInterfaceLang :: UI_Lang -> UI_Lang
    findInterfaceLang UI_French = UI_French
    findInterfaceLang _         = UI_English


serveStatic :: Static.CacheContainer -> [(String, FP.FilePath)] -> WebM ()
serveStatic cache parts =
  let opts = Static.defaultOptions { Static.cacheContainer = cache }
      basePolicy = mconcat [ Static.noDots
                           , Static.isNotAbsolute
                           , Static.predicate (not . isInfixOf "/.")
                           ]
      contentPolicies = map (\(url,path) -> contentPolicy url path) parts
      policy = basePolicy >-> foldl1 (<|>) contentPolicies
  in middleware $
     Static.staticPolicyWithOptions opts policy
  where
    contentPolicy url fspath = mconcat [ Static.policy (stripPrefix url)
                                       , Static.addBase fspath ]


