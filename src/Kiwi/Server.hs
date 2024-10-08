{-# LANGUAGE OverloadedStrings #-}

module Kiwi.Server
    ( kiwiServer
    ) where


import           Control.Concurrent (forkIO)
import           Control.Monad.Reader
import           Data.IORef (newIORef, readIORef, atomicWriteIORef)
import           Data.List (stripPrefix, isInfixOf)
-- import           Data.Maybe (catMaybes)
import qualified Data.Map.Strict as M
import qualified Data.SearchEngine as SE
-- import           Data.Text (Text, intercalate)
-- import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
-- import qualified Network.Wai as WAI
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Network.Wai.Middleware.Static as Static
import           Network.Wai.Middleware.Static ((>->), (<|>))
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.Mustache as X
import           Text.Printf (printf)
import           Web.Scotty.Trans

import qualified Kiwi.ConfigFile as Conf
import           Kiwi.Controller
import qualified Kiwi.PagesDB as DB
import           Kiwi.Types
import           Kiwi.Utils
import qualified Utils.DocIndex as DI

-- gets :: (AppState -> a) -> ServerM a
-- gets f = ask >>= return . f

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
      putStrLn $ concat ["Kiwi scaned ", nb, " pages and 1 theme in ", ts, "s"]


kiwiRoute :: ServerState -> Static.CacheContainer -> WebM ()
kiwiRoute ss cache = do
  get "/" $ redirect "/browse/"
  get "/page/:source/:pId" $ do
    source <- param "source"
    pId <- param "pId"
    withLogin $ servePage (source, pId)
  -- getPath "^/page/:source/[^.]+" $ \p -> withLogin $ do
    
  --   servePage p
  -- get "/browse/tags/:tags" $ withLogin $ do
  --   tags <- param "tags"
  --   serveTagged tags
  -- get "/browse-meta/:meta/:keys" $ withLogin $ do
  --   meta <- param "meta"
  --   keys <- param "keys"
  --   serveBrowseMeta meta keys
  get "/browse/:req" $ withLogin $ do
    req <- TL.toStrict <$> param "req"
    serveBrowseAll req
  get "/search" $ withLogin $ do
    query <- param "query"
    serveSearch query
  post "/reload" $ ifAdmin $ do
    withLogin updateDB
    html ""
  post "/edit-page" $ ifAdmin $ do
    source <- param "page-source"
    pId <- param "page-id"
    serveEditPage (editorCommand ss) (source, pId)
  get  "/login" $ serveLogin
  post "/login" $ logUserIn
  post "/logout" $ logUserOut
  get "/show-index" $ ifAdmin $ do
    db <- getDB
    html $ TL.pack $ show $ DI.index $ pagesIndex db

  serveStatic cache [("static/", staticDir ss)]

  forM_ (contentSources ss) $ \src ->
    serveStatic cache $ filesParts (contentDir ss) src

  notFound serveNotFound

  where
    filesParts cd src = [ ( concat ["image/", src, "/"]
                          , FP.joinPath [cd, src, imagesFSDir] )
                        , ( concat ["file/", src, "/"]
                          , FP.joinPath [cd, src, filesFSDir]) ]
    


updateDB :: ActM ()
updateDB = do
  dbR <- serverM $ asks pagesDB
  db' <- liftIO $ readIORef dbR
  cd <- asksK contentDir
  db  <- liftAndCatchIO $ DB.updatePagesDB cd pagesFSDir db'
  liftAndCatchIO $ atomicWriteIORef dbR db


initServerState :: FP.FilePath -> Conf.KiwiConfig -> IO ServerState
initServerState cfp conf = do
  kiwiDir' <- D.makeAbsolute (FP.takeDirectory cfp)
  let contentDir' = FP.combine kiwiDir' (Conf.contentDir conf)
  sources <- filter (not . isDotFile) <$> D.listDirectory contentDir'
  let staticDir'  = FP.joinPath [kiwiDir', (Conf.themeDir conf), "static"]
  -- let pagesRootDir = FP.combine contentDir' pagesFSDir
  tpl <- compileTemplate $ FP.joinPath [kiwiDir', (Conf.themeDir conf), "mustache"]
  accounts' <- loadAccounts $ FP.combine kiwiDir' "_accounts"
  sess      <- loadSessions $ FP.combine kiwiDir' "_sessions"
  sessR <- newIORef sess
  db <- DB.updatePagesDB contentDir' pagesFSDir
          (DB.emptyPagesDB (Conf.defaultMeta conf)
                           (Conf.customMetaConfig conf)
                           (Conf.sourcesConfig conf))
  _ <- warmUpSearchEngine (searchEngine db)
  dbR <- newIORef db
  return $ ServerState {
      kiwiName   = Conf.name conf
    , contentDir = contentDir'
    , contentSources = sources
    , staticDir  = staticDir'
    , kiwiDir    = kiwiDir'
    , editorCommand = Conf.editor conf
    , uiLang       = findInterfaceLang (Conf.uiLang conf)
    , template     = tpl
    , accounts     = accounts'
    , sessions     = sessR
    , login        = Nothing
    , pagesDB      = dbR
    , tocSetting   = Conf.toc conf
  }
  where
    findInterfaceLang :: UI_Lang -> UI_Lang
    findInterfaceLang UI_French = UI_French
    findInterfaceLang _         = UI_English

    warmUpSearchEngine se = forkIO $ do   -- deep eval of search engine
      True <- return $ SE.invariant se
      return ()


compileTemplate :: FP.FilePath -> IO KiwiTemplate
compileTemplate dir =
  KiwiTemplate <$>
  make "layout" <*>
  make "home" <*>
  make "login" <*>
  make "page" <*>
  make "tagged" <*>
  make "browse" <*>
  make "agenda" <*>
  make "search-results" <*>
  make "not-found" <*>
  make "forbidden"
  where
    make t = X.compileMustacheDir t dir


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


