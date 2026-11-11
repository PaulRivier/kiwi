module Kiwi (runKiwiRun) where

import           Control.Concurrent.Async (mapConcurrently_)
import           Control.Exception (bracket_, try)
import           Control.Monad (forM_, unless, when)
import qualified Data.ByteString as BS
import qualified Data.List as L
import           Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Version (showVersion)
import qualified System.Directory as D
import           System.Environment (getExecutablePath)
import           System.Exit (exitFailure, exitWith)
import qualified System.FilePath as FP
import           System.IO
import           System.IO.Error (isEOFError)
import           System.Process (rawSystem)
import           GHC.IO.Encoding (setLocaleEncoding, setFileSystemEncoding)

import Paths_kiwi (version)

import qualified Kiwi.Auth as Auth
import Kiwi.CLI as CLI
import Kiwi.ConfigFile as Conf
import qualified Kiwi.DefaultPages as DP
import qualified Kiwi.Log as L
import Kiwi.Server (kiwiServer)
import qualified Kiwi.Utils as U
import Kiwi.Types


runKiwiRun :: IO ()
runKiwiRun = do
  -- kiwi files (pages, config) are UTF-8, whatever the
  -- system locale is (it may be C/POSIX for services, containers ...)
  setLocaleEncoding utf8
  -- file names too, roundtrip keeps non UTF-8 names usable
  setFileSystemEncoding =<< mkTextEncoding "UTF-8//ROUNDTRIP"
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  -- logs are written at once, even when stdout is not a terminal (journald ...)
  hSetBuffering stdout LineBuffering
  cli <- CLI.parseCLI
  case cli of
    Server c -> runServers (CLI.serverLogLevel c) (CLI.serverCfps c)
    Init c -> initKiwiDir (CLI.initDirectory c)
    Access action -> runAccess action
    Update c -> runUpdate c


-- | Every config is read and checked before any server starts, so that
-- one error stops them all with a failure code. Once started, a server that
-- stops on an error (a port in use, an unreadable accounts file ...) stops
-- the others, and its exception (or exit code) ends the program.
runServers :: L.LogLevel -> [FP.FilePath] -> IO ()
runServers logLevel cfps = do
  confsM <- mapM (loadServerConfig logLevel) cfps
  confs <- maybe exitFailure return $ sequence confsM
  let ports = map (Conf.port . snd) confs
      dupPorts = U.nodup $ ports L.\\ U.nodup ports
  unless (null dupPorts) $ do
    L.mkLogger logLevel "?" L.Error $ T.pack $
      "several kiwis use the same port: " ++ L.intercalate ", " (map show dupPorts)
    exitFailure
  cache <- U.initWAICaching
  mapConcurrently_ (\(cfp, conf) -> kiwiServer cfp conf logLevel cache) confs


-- | The config of a kiwi, if it can be read and its content directory
-- exists. Errors are logged.
loadServerConfig :: L.LogLevel -> FP.FilePath -> IO (Maybe (FP.FilePath, Conf.KiwiConfig))
loadServerConfig logLevel cfp = do
  cfgTextM <- U.getFileContent cfp
  case cfgTextM of
    Nothing -> do
      logError $ "config file " ++ cfp ++ " does not exist"
      return Nothing
    Just cfgText -> do
      case Conf.parseConfig cfgText of
        Left e -> do
          logError $ "error while reading config file " ++ cfp ++ " : " ++ e
          let kd = FP.takeDirectory cfp
          logError $ "a clean config is written to "
                     ++ FP.combine kd "kiwi.ini.default" ++ " to help you fix it"
          BS.writeFile (FP.combine kd "kiwi.ini.default") Conf.defaultConfigFileContent
          return Nothing
        Right conf -> do
          -- relative to the folder of the config file, as in the server
          let cd = FP.combine (FP.takeDirectory cfp) (Conf.contentDir conf)
          cdExists <- D.doesDirectoryExist cd
          if cdExists then return $ Just (cfp, conf)
          else do
            L.mkLogger logLevel (Conf.name conf) L.Error $ T.pack $
              "content directory " ++ cd ++ " (config file " ++ cfp ++ ") does not exist"
            return Nothing
  where
    -- the kiwi name is unknown until the config file is read
    logError = L.mkLogger logLevel "?" L.Error . T.pack


initKiwiDir :: FP.FilePath -> IO ()
initKiwiDir d = do
  exists <- D.doesDirectoryExist d
  case exists of
    False -> exitError "Directory does not exist, check your path"
    True -> do
      clean  <-  filter (not . U.isDotFile) <$> D.listDirectory d
      case null clean of
        False -> do
          BS.writeFile (FP.combine d "kiwi.ini.default") Conf.defaultConfigFileContent
          exitError $ L.intercalate "\n"
            ["Directory must be empty for initialisation",
             "In case you want to update your kiwi.ini file, ",
             "kiwi copied current kiwi.ini file format to kiwi.ini.default"]
        True -> createKiwi d
  where
    createKiwi r = do
      forM_ (dirs r) D.createDirectory
      BS.writeFile (FP.combine r "kiwi.ini") Conf.defaultConfigFileContent
      BS.writeFile (FP.combine r "content/my/pages/Home.md") DP.defaultHomePage
      BS.writeFile (FP.combine r "content/my/pages/CommonMark.md") DP.commonmarkDocPage
      putStrLn $ unlines ["Kiwi directory created at " ++ r,
                          "You should now do the following steps :",
                          " - check kiwi.ini file",
                          " - start wiki with command : kiwi serve " ++ FP.combine r "kiwi.ini"]

    dirs r = map (FP.combine r) ["content", "content/my", "content/my/pages",
                                 "content/my/images", "content/my/files"]
      


-- | Create, update or remove an account. The accounts file is replaced at
-- once (see 'Auth.dumpAccounts'), and running servers read it again at each
-- login and reload, so there is no need to restart them. A new salt is made
-- with each new password, which closes the sessions opened with the former
-- one. Any error exits with a failure code.
runAccess :: AccessConfig -> IO ()
runAccess c = do
  let cfp = accessCfp c
      accountsFilePath = FP.combine (FP.takeDirectory cfp) "_accounts"
  cfgExists <- D.doesFileExist cfp
  unless cfgExists $ exitError $ "config file " ++ cfp ++ " does not exist"
  (accounts', refused) <- Auth.loadAccounts accountsFilePath
  -- saving the accounts would remove the lines that are not read
  forM_ (Auth.refusedLinesWarning accountsFilePath refused) $ \w ->
    exitError $ w ++ ". Fix or remove these lines first."
  let findAccount l = L.find (\a -> accountId a == l) accounts'
      save accs l what = do
        warningM <- Auth.dumpAccounts accs accountsFilePath
        mapM_ (hPutStrLn stderr) warningM
        putStrLn $ concat [ "Account ", T.unpack l, " ", what, ". Running kiwi servers apply "
                          , "the change at the next login or reload (admin reload button)." ]
  case accessOp c of
    AccessCreate l -> do
      when (isJust $ findAccount l) $
        exitError $ "account " ++ T.unpack l ++ " already exists, use --update to change it"
      passM <- askPassword False
      pass <- maybe (exitError "the password can not be empty") return passM
      groups <- askGroups Nothing
      acc <- newAccount l pass groups
      save (accounts' ++ [acc]) l "created"
    AccessUpdate l -> case findAccount l of
      Nothing -> exitError $ "account " ++ T.unpack l ++ " does not exist, use --create to create it"
      Just old -> do
        passM <- askPassword True
        groups <- askGroups (Just $ accountGroups old)
        acc <- case passM of
          Nothing -> return old { accountGroups = groups }
          Just pass -> newAccount l pass groups
        save (map (\a -> if accountId a == l then acc else a) accounts') l "updated"
    AccessRemove l -> case findAccount l of
      Nothing -> exitError $ "account " ++ T.unpack l ++ " does not exist"
      Just _ -> save (filter (\a -> accountId a /= l) accounts') l "removed"
  where
    newAccount l pass groups = do
      salt <- Auth.randomText 16
      case Auth.hashPassword salt pass of
        Nothing -> exitError "unable to hash the password"
        Just hash -> return $ Account l salt hash groups

-- | The new password, asked twice. Nothing when it is left empty, which
-- keeps the current one if allowed.
askPassword :: Bool -> IO (Maybe T.Text)
askPassword canKeep = do
  p1 <- askLine True $ if canKeep then "Password (empty to keep it): " else "Password: "
  if T.null p1 then return Nothing
  else do
    p2 <- askLine True "Password again: "
    if p1 /= p2 then exitError "the passwords do not match"
    else return (Just p1)

-- | Groups of the account. With the current groups, an empty answer keeps
-- them.
askGroups :: Maybe [T.Text] -> IO [T.Text]
askGroups currentM = do
  t <- askLine False $ case currentM of
    Nothing -> "Groups (comma separated): "
    Just gs -> "Groups [" ++ T.unpack (T.intercalate "," gs) ++ "]: "
  case currentM of
    Just gs | T.null (T.strip t) -> return gs
    _ -> either exitError return $ Auth.parseGroups t

-- | Ask a line on the terminal, hidden or not
askLine :: Bool -> String -> IO T.Text
askLine hidden question = do
  putStr question
  hFlush stdout
  r <- try $ if hidden then noEcho TIO.getLine else TIO.getLine
  -- the newline typed after a hidden input is not displayed
  when hidden $ putStrLn ""
  case r of
    Right l -> return l
    Left e | isEOFError e -> exitError "unexpected end of input"
           | otherwise -> ioError e

-- | Error of kiwi init or kiwi access: message on stderr, failure code
exitError :: String -> IO a
exitError msg = do
  hPutStrLn stderr $ "error: " ++ msg
  exitFailure

-- | The update is done by scripts/update-kiwi-linux.sh, downloaded from the
-- latest release, so that it always matches the format of the release. It
-- runs in the terminal of kiwi (it asks for a confirmation), with the path
-- and the version of this binary.
runUpdate :: UpdateConfig -> IO ()
runUpdate c = do
  exe <- getExecutablePath
  code <- rawSystem "bash" $ ["-c", bootstrap, "kiwi-update"]
                             ++ ["--check" | CLI.updateCheck c]
                             ++ [exe, showVersion version]
  exitWith code
  where
    url = "https://github.com/PaulRivier/kiwi/releases/latest/download/update-kiwi-linux.sh"
    -- the script is saved before it runs: a partial download runs nothing,
    -- and its standard input stays the terminal
    bootstrap = unlines
      [ "set -e"
      , "command -v curl > /dev/null || { echo 'error: kiwi update needs curl' >&2; exit 1; }"
      , "tmp=\"$(mktemp -d)\""
      , "trap 'rm -rf -- \"$tmp\"' EXIT"
      , "curl -fsSL -o \"$tmp/update.sh\" " ++ url ++ " || \
        \{ echo 'error: unable to download the update script of the latest release' >&2; exit 1; }"
      , "bash \"$tmp/update.sh\" \"$@\"" ]

noEcho :: IO a -> IO a
noEcho action = do
  bracket_ (hSetEcho stdin False)
           (hSetEcho stdin True)
           action

          
