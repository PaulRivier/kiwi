module TestUtils (defMeta, load, loadIn, mkPage, mkPagesDB, withContentDir, withKiwiApp,
                  withKiwiAccounts) where

import           Control.Exception (bracket, try)
import           Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Network.Wai as Wai
import qualified System.Directory as D
import qualified System.FilePath as FP
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (isAlreadyExistsError)

import qualified Kiwi.Auth as Auth
import qualified Kiwi.ConfigFile as Conf
import qualified Kiwi.PagesDB as DB
import           Kiwi.Pandoc (loadPage)
import           Kiwi.Server (initServerState, kiwiApp)
import           Kiwi.Types
import qualified Kiwi.Utils as U
import qualified Utils.DocIndex as DI

defMeta :: MetaData
defMeta = MetaData { metaId = Nothing
                   , metaTitle = "untitled"
                   , metaTags = ["untagged"]
                   , metaAccess = ["private"]
                   , metaLang = LangFixed English
                   , metaArchive = False
                   , metaCustom = []
                   }

-- | A page of a source, with its tags, access groups, archive flag, and
-- modification day (in january 2026)
mkPage :: PageUID -> [TagId] -> [T.Text] -> Bool -> Int -> Page
mkPage uid tags access archive day =
  Page { pageUID = uid
       , pageAbsoluteFSPath = "/dev/null"
       , pageMTime = Time.UTCTime (Time.fromGregorian 2026 1 day) 0
       , pageBody = ""
       , pageImagesDir = ""
       , pageFilesDir = ""
       , pageTitle = snd uid
       , pageTags = tags
       , pageLang = English
       , pageAccess = access
       , pageLinks = []
       , pageArchive = archive
       , pageCustomMeta = []
       }

-- | Database of these pages, indexed as by 'DB.updatePagesDB'
mkPagesDB :: [Page] -> PagesDB
mkPagesDB pages = let db = DB.emptyPagesDB defMeta [] []
                  in db { pagesIndex = DI.insertMany (pagesIndex db) pages }


-- | Load a page from source "src", located in pages subfolder "sub"
load :: T.Text -> PandocPage
load = loadIn "sub"

loadIn :: FilePath -> T.Text -> PandocPage
loadIn dir c = case loadPage "test.md" c "src" defMeta [] dir of
  Left e -> error e
  Right p -> p

-- | Runs the test on the content folder of a new temporary kiwi folder,
-- removed afterwards
withContentDir :: (FilePath -> IO a) -> IO a
withContentDir test = do
  tmp <- D.getTemporaryDirectory
  bracket (newDir tmp (0 :: Int)) D.removeDirectoryRecursive $ \d -> do
    let cd = d FP.</> "content"
    D.createDirectoryIfMissing True (cd FP.</> "s" FP.</> "pages")
    test cd
  where
    newDir tmp n = do
      let d = tmp FP.</> ("kiwi-test-" ++ show n)
      r <- try $ D.createDirectory d
      case r of
        Right () -> return d
        Left e | isAlreadyExistsError e -> newDir tmp (n + 1)
               | otherwise -> ioError e

-- | Runs the test on the application of a new temporary kiwi, with the given
-- files (name, content) as the pages of its source "s". The default access
-- of the pages is "public".
withKiwiApp :: [(FilePath, String)] -> (Wai.Application -> IO a) -> IO a
withKiwiApp = withKiwiAccounts []

-- | 'withKiwiApp', with accounts: (login, password, groups)
withKiwiAccounts :: [(T.Text, T.Text, [T.Text])] -> [(FilePath, String)] ->
                    (Wai.Application -> IO a) -> IO a
withKiwiAccounts accounts' files test = withContentDir $ \cd -> do
  let kiwiDir' = FP.takeDirectory cd
      account (l, pass, groups) = case Auth.hashPassword "kiwi-test-salt00" pass of
        Just hash -> return $ Account l "kiwi-test-salt00" hash groups
        Nothing -> fail "unable to hash the password"
  forM_ files $ \(name, c) -> writeFile (cd FP.</> "s" FP.</> "pages" FP.</> name) c
  accs <- mapM account accounts'
  mapM_ (hPutStrLn stderr) =<< Auth.dumpAccounts accs (kiwiDir' FP.</> "_accounts")
  conf <- either fail return $ Conf.parseConfig $ T.unlines
    [ "[kiwi]", "content-directory: content", "port: 3456"
    , "[metadata-default]", "access: public" ]
  ss <- initServerState (kiwiDir' FP.</> "kiwi.ini") conf (\_ _ -> return ())
  cache <- U.initWAICaching
  kiwiApp ss cache >>= test
