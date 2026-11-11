module Kiwi.CLI (Config(..), ServerConfig(..), InitConfig(..), AccessConfig(..), AccessOp(..),
                 UpdateConfig(..), ReloadConfig(..), parseCLI ) where

-- Afficher version Cabal
import Paths_kiwi (version)
import Data.Version (showVersion)

import Data.Char (toLower)
import qualified Data.Text as T
import Options.Applicative
import qualified System.FilePath as FP

import Kiwi.Auth (validateLogin)
import qualified Kiwi.Log as L

data Config = Server ServerConfig
            | Init InitConfig
            | Access AccessConfig
            | Update UpdateConfig
            | Reload ReloadConfig


data ServerConfig =
  ServerConfig { serverLogLevel :: L.LogLevel
               , serverCfps :: [FP.FilePath] }

data InitConfig =
  InitConfig { initDirectory :: FP.FilePath }

data AccessConfig =
  AccessConfig { accessCfp :: FP.FilePath
               , accessOp  :: AccessOp }

-- | Operation on the account of a login (validated by 'validateLogin')
data AccessOp = AccessCreate T.Text
              | AccessUpdate T.Text
              | AccessRemove T.Text

data UpdateConfig =
  UpdateConfig { updateCheck :: Bool }

data ReloadConfig =
  ReloadConfig { reloadCfp :: FP.FilePath }

parser :: Parser Config
parser = hsubparser
         ( command "serve" (info serveParser (progDesc "Start your kiwi server, serve one or more directory"))   <>
           command "init"  (info initParser (progDesc "Init a new kiwi directory")) <>
           command "access" (info accessParser (progDesc "Manage accounts and permissions")) <>
           command "reload" (info reloadParser (progDesc "Ask the running kiwi server of a \
                                                         \config file to reload its pages \
                                                         \and accounts")) <>
           command "update" (info updateParser (progDesc "Update kiwi to its latest release \
                                                         \(Linux x86_64 only, needs bash, curl, \
                                                         \tar and sha256sum)")) )
  where
    serveParser :: Parser Config
    serveParser =  Server <$> (ServerConfig <$> logLevelParser <*> configFilePathsParser)

    initParser  :: Parser Config
    initParser  =  Init <$> (InitConfig <$> initDirParser)

    accessParser :: Parser Config
    accessParser = Access <$>
      ( AccessConfig <$> configFilePathParser <*>
        ( ( AccessCreate <$> option readLogin ( long "create"
                                                <> metavar "LOGIN"
                                                <> help "create new account with login" ) ) <|>
          ( AccessUpdate <$> option readLogin ( long "update"
                                                <> metavar "LOGIN"
                                                <> help "change the password or the groups \
                                                        \of an account" ) ) <|>
          ( AccessRemove <$> option readLogin ( long "remove"
                                                <> metavar "LOGIN"
                                                <> help "remove account with login" ) ) )
      )

    reloadParser :: Parser Config
    reloadParser = Reload <$> (ReloadConfig <$> configFilePathParser)

    updateParser :: Parser Config
    updateParser = Update <$>
      ( UpdateConfig <$> switch ( long "check"
                                  <> help "only tell whether a newer release exists" ) )


    configFilePathParser = argument str ( metavar "CONFIG_FILE"
                                          <> help "Path to your kiwi config file" )

    configFilePathsParser = some $
      argument str ( metavar "CONFIG_FILE [CONFIG_FILE]"
                     <> help "Paths to your kiwi config files (one or more)" )

    initDirParser = argument str ( metavar "KIWI_DIRECTORY"
                                   <> help "Path to your new kiwi directory, \
                                           \ must be an existing empty folder" )

    logLevelParser = option readLogLevel ( long "log-level"
                                           <> metavar "LEVEL"
                                           <> value L.Info
                                           <> help "error, warn, info (default) or debug" )

    readLogLevel = maybeReader $ \l -> case map toLower l of
      "error" -> Just L.Error
      "warn"  -> Just L.Warn
      "info"  -> Just L.Info
      "debug" -> Just L.Debug
      _       -> Nothing

    readLogin = eitherReader (validateLogin . T.pack)


spec :: ParserInfo Config
spec = info (helper <*> versionOption <*> parser)
            (fullDesc
            <> progDesc (concat ["Version ", showVersion version, ". Kiwi is a powerful personal wiki system."])
            <> header "Kiwi")
  where
    -- the update script reads the version as the last word
    versionOption = infoOption ("kiwi " ++ showVersion version)
                               ( long "version" <> help "Show the version" )

parseCLI :: IO Config
parseCLI = execParser spec




