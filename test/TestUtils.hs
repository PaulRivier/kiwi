module TestUtils (defMeta, display, displayWith, load, mkPage, mkPagesDB, mkPagesDBWith, withContentDir, withKiwiApp,
                  withKiwiAccounts, withKiwiState, withKiwiConfig, withGit, git, gitOut) where

import           Control.Exception (bracket, try)
import           Control.Monad (forM_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Network.Wai as Wai
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified System.Process as Proc
import           Test.Hspec (pendingWith)
import           System.IO (hPutStrLn, stderr)
import           System.IO.Error (isAlreadyExistsError)

import qualified Kiwi.Auth as Auth
import qualified Kiwi.ConfigFile as Conf
import qualified Kiwi.PagesDB as DB
import           Kiwi.Meta (emptyMeta)
import           Kiwi.Pandoc (loadPage, pageDoc)
import qualified Text.Pandoc.Definition as PD
import           Kiwi.Server (initServerState, kiwiApp)
import           Kiwi.Types
import qualified Utils.DocIndex as DI
import qualified Utils.DocIndex.Restricted as RDI

defMeta :: MetaData
defMeta = emptyMeta { metaTitle = Just "untitled"
                    , metaTags = Just ["untagged"]
                    , metaAccess = Just [("private", AccessRead)]
                    , metaLang = Just $ LangFixed English
                    }

-- | A page of a source, with its tags, access groups, archive flag, and
-- modification day (in january 2026)
mkPage :: PageUID -> [TagId] -> [T.Text] -> Bool -> Int -> Page
mkPage uid tags access archive day =
  Page { pageUID = uid
       , pageAbsoluteFSPath = "/dev/null"
       , pageMTime = Time.UTCTime (Time.fromGregorian 2026 1 day) 0
       , pageBody = ""
       , pageContentSize = 0
       , pageContentLines = 0
       , pageFiles = []
       , pagePlainText = ""
       , pageHeadings = []
       , pageTitle = snd uid
       , pageSummary = Nothing
       , pageTags = tags
       , pageLang = English
       , pageAccess = map (, AccessRead) access
       , pageLinks = []
       , pageArchive = archive
       , pageCustomMeta = []
       , pagePreviousUids = []
       }

-- | Database of these pages, indexed as by 'DB.updatePagesDB'
mkPagesDB :: [Page] -> PagesDB
mkPagesDB = mkPagesDBWith []

-- | 'mkPagesDB', with custom metadata fields
mkPagesDBWith :: [CustomMetaConfig] -> [Page] -> PagesDB
mkPagesDBWith cmc pages = let db = DB.emptyPagesDB defMeta cmc []
                  in db { pagesIndex = RDI.whole $ DI.insertMany (RDI.visibleIndex $ pagesIndex db) pages }


-- | Document of a page content, as displayed (see 'pageDoc')
display :: T.Text -> PD.Pandoc
display = displayWith []

-- | 'display', with these files of the source "src": (uid, path)
displayWith :: [(T.Text, FilePath)] -> T.Text -> PD.Pandoc
displayWith files body =
  either error id $ pageDoc (M.fromList [ (("src", u), p) | (u, p) <- files ])
    (mkPage ("src", "p") [] [] False 1) { pageAbsoluteFSPath = "/k/content/src/p.md"
                                        , pageBody = body }

-- | Load a page from source "src"
load :: T.Text -> PandocPage
load c = case loadPage "test.md" c "src" defMeta [] of
  Left e -> error e
  Right p -> p

-- | Runs the test on the content folder of a new temporary kiwi folder,
-- removed afterwards
withContentDir :: (FilePath -> IO a) -> IO a
withContentDir test = do
  tmp <- D.getTemporaryDirectory
  bracket (newDir tmp (0 :: Int)) D.removeDirectoryRecursive $ \d -> do
    let cd = d FP.</> "content"
    D.createDirectoryIfMissing True (cd FP.</> "s")
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
withKiwiAccounts accounts' files test = withKiwiState accounts' files $ \_ _ -> test

-- | 'withKiwiAccounts', with the content folder and the state of the kiwi
withKiwiState :: [(T.Text, T.Text, [T.Text])] -> [(FilePath, String)] ->
                 (FilePath -> ServerState -> Wai.Application -> IO a) -> IO a
withKiwiState = withKiwiConfig []

-- | 'withKiwiState', with more lines in the [kiwi] section of its config
withKiwiConfig :: [T.Text] -> [(T.Text, T.Text, [T.Text])] -> [(FilePath, String)] ->
                  (FilePath -> ServerState -> Wai.Application -> IO a) -> IO a
withKiwiConfig kiwiLines accounts' files test = withContentDir $ \cd -> do
  let kiwiDir' = FP.takeDirectory cd
      account (l, pass, groups) = case Auth.hashPassword "kiwi-test-salt00" pass of
        Just hash -> return $ Account l "kiwi-test-salt00" hash groups
        Nothing -> fail "unable to hash the password"
  forM_ files $ \(name, c) -> do
    let path = cd FP.</> "s" FP.</> name
    D.createDirectoryIfMissing True (FP.takeDirectory path)
    writeFile path c
  accs <- mapM account accounts'
  mapM_ (hPutStrLn stderr) =<< Auth.dumpAccounts accs (kiwiDir' FP.</> "_accounts")
  conf <- either fail return $ Conf.parseConfig $ T.unlines $
    [ "[kiwi]", "content-directory: content", "port: 3456" ] ++ kiwiLines ++
    [ "[metadata-default]", "access: public"
    , "[metadata zone]", "type: text", "description: the area"
    , "[metadata author]", "type: text" ]
  ss <- initServerState (kiwiDir' FP.</> "kiwi.ini") conf (\_ _ -> return ())
  kiwiApp ss >>= test cd ss

-- | Runs the test when git is installed, else marks it pending
withGit :: IO () -> IO ()
withGit test = D.findExecutable "git" >>= maybe (pendingWith "git is not installed") (const test)

git :: FilePath -> [String] -> IO ()
git dir args = () <$ gitOut dir args

-- | Output of git in this folder, with an identity for the commits of the
-- tests
gitOut :: FilePath -> [String] -> IO String
gitOut dir args = Proc.readCreateProcess
  (Proc.proc "git" (["-c", "user.name=test", "-c", "user.email=test@test"] ++ args))
    { Proc.cwd = Just dir } ""
