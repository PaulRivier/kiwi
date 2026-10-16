module Kiwi.Server
    ( kiwiServer
    ) where


import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar (newMVar, withMVar)
import           Control.Monad (when, forM_)
import           Control.Monad.Reader
import           Data.IORef (newIORef, readIORef, atomicWriteIORef)
import           Data.List (stripPrefix, isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.SearchEngine as SE
import qualified Data.Text.Lazy as TL
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Static
import           Network.Wai.Middleware.Static ((>->), (<|>))
import qualified System.Directory as D
import qualified System.FilePath as FP
import           Text.Printf (printf)
import           Web.Scotty.Trans

import qualified Kiwi.ConfigFile as Conf
import           Kiwi.Controller
import qualified Kiwi.PagesDB as DB
import           Kiwi.Theme (serveTheme)
import           Kiwi.Types
import           Kiwi.Utils
import qualified Utils.DocIndex as DI


pagesFSDir, imagesFSDir, filesFSDir :: FilePath
pagesFSDir = "pages"
filesFSDir = "files"
imagesFSDir = "images"


kiwiServer :: FP.FilePath -> Conf.KiwiConfig ->
              Static.CacheContainer -> IO ()
kiwiServer cfp c cache = do
  (ss, t) <- timeIO $ initServerState cfp c
  displayStartupTime ss t
  let runActionToIO m = runReaderT (runServerM m) ss
  scottyT (Conf.port c) runActionToIO $ do
    when (Conf.logging c) $ middleware logStdoutDev
    kiwiRoute ss cache
  where
    displayStartupTime ss t = do
      let ts = printf "%.2f" t
      nb <- (show . M.size . DI.store . pagesIndex) <$> liftIO (readIORef $ pagesDB ss)
      putStrLn $ concat ["Kiwi scaned ", nb, " pages in ", ts, "s"]


kiwiRoute :: ServerState -> Static.CacheContainer -> WebM ()
kiwiRoute ss cache = do
  get "/" $ redirect "/browse"
  get "/page/:source/:pId" $ do
    source <- pathParam "source"
    pId <- pathParam "pId"
    withLogin $ servePage (source, pId)
  get "/browse" $ withLogin $ queryParams >>= serveBrowseAll
  get "/search" $ withLogin $ do
    query <- queryParam "query"
    serveSearch query
  post "/reload" $ ifAdmin $ do
    withLogin updateDB
    html ""
  post "/edit-page" $ ifAdmin $ do
    source <- formParam "page-source"
    pId <- formParam "page-id"
    serveEditPage (editorCommand ss) (source, pId)
  get  "/login" $ serveLogin
  post "/login" $ logUserIn
  post "/logout" $ logUserOut
  get "/show-index" $ ifAdmin $ do
    db <- getDB
    html $ TL.pack $ show $ DI.index $ pagesIndex db

  middleware serveTheme

  forM_ (contentSources ss) $ \src ->
    serveStatic cache $ filesParts (contentDir ss) src

  notFound serveNotFound

  where
    filesParts cd src = [ ( concat ["image/", src, "/"]
                          , FP.joinPath [cd, src, imagesFSDir] )
                        , ( concat ["file/", src, "/"]
                          , FP.joinPath [cd, src, filesFSDir]) ]
    


-- | Reloads are serialized by pagesDBLock, so that two concurrent reloads
-- can not overwrite each other. Readers are never blocked.
updateDB :: ActM ()
updateDB = do
  dbR <- asksK pagesDB
  lock <- asksK pagesDBLock
  cd <- asksK contentDir
  liftIO $ withMVar lock $ \_ -> do
    db' <- readIORef dbR
    db  <- DB.updatePagesDB cd pagesFSDir db'
    atomicWriteIORef dbR db


initServerState :: FP.FilePath -> Conf.KiwiConfig -> IO ServerState
initServerState cfp conf = do
  kiwiDir' <- D.makeAbsolute (FP.takeDirectory cfp)
  let contentDir' = FP.combine kiwiDir' (Conf.contentDir conf)
  sources <- filter (not . isDotFile) <$> D.listDirectory contentDir'
  accounts' <- loadAccounts $ FP.combine kiwiDir' "_accounts"
  sess      <- loadSessions $ FP.combine kiwiDir' "_sessions"
  sessM <- newMVar sess
  db <- DB.updatePagesDB contentDir' pagesFSDir
          (DB.emptyPagesDB (Conf.defaultMeta conf)
                           (Conf.customMetaConfig conf)
                           (Conf.sourcesConfig conf))
  _ <- warmUpSearchEngine (searchEngine db)
  dbR <- newIORef db
  dbLock <- newMVar ()
  return $ ServerState {
      kiwiName   = Conf.name conf
    , contentDir = contentDir'
    , contentSources = sources
    , kiwiDir    = kiwiDir'
    , editorCommand = Conf.editor conf
    , uiLang       = findInterfaceLang (Conf.uiLang conf)
    , accounts     = accounts'
    , sessions     = sessM
    , login        = Nothing
    , pagesDB      = dbR
    , pagesDBLock  = dbLock
    , tocSetting   = Conf.toc conf
    , secureCookie = Conf.secureCookie conf
  }
  where
    findInterfaceLang :: UI_Lang -> UI_Lang
    findInterfaceLang UI_French = UI_French
    findInterfaceLang _         = UI_English

    warmUpSearchEngine se = forkIO $ do   -- deep eval of search engine
      True <- return $ SE.invariant se
      return ()


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


